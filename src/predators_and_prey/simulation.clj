(ns predators-and-prey.simulation
	(:use predators-and-prey.constants)
	(:use predators-and-prey.collisions)
        (:use predators-and-prey.vectors)
(:use [clojure.pprint :only [pprint]])
        )

(def predator {:max-velocity 8 :radius 20})
(def prey {:max-velocity 5 :radius 10})

(def animals (atom {}))

(defn create-predator [x y horizontal-velocity vertical-velocity]
	(conj {:x x :y y :vx horizontal-velocity :vy vertical-velocity} predator))
	
(defn create-prey
	([screen-size]
	(conj {:x (rand-int screen-size) :y (rand-int screen-size) :vx (rand-int (:max-velocity prey)) :vy (rand-int (:max-velocity prey))} prey))
	([x y vx vy]
	(conj {:x x :y y :vx vx :vy vy} prey)))
	
(defn prey-generator [screen-size]
	#(conj {:x (rand-int screen-size) :y (rand-int screen-size) :vx (rand-int (:max-velocity prey)) :vy (rand-int (:max-velocity prey))} prey))
	
(defn initial-state []
	{:predators [(create-predator 50 50 4 4)
	(create-predator (- screen-size 100) (- screen-size 100) -4 -4)]
	:prey (take 50 (repeatedly (prey-generator screen-size)))})

(defn move [animal]
	(let [x (:x animal) y (:y animal)
	vx (:vx animal) vy (:vy animal)]
	(assoc animal :x (mod (+ x vx) screen-size) :y (mod (+ y vy) screen-size))))

(defn surviving? [predators prey]
  (not-any? #(collides? prey %) predators))  ; replaced function that returns a function with a simple function
                                         ; the call to the function is modified to a partial application
                                         ; I did this mostly because I think anything that ends with a question mark
                                         ; should return a boolean
                                         ; Also replaced some/nil?/if with not-any?

(def flee -)
(def target +)

(defn move-towards [animal point strategy]
  (let [difference (sub point [(:x animal) (:y animal)])
        unit-vec (unit difference)
        [vx vy] (mul (strategy (:max-velocity animal)) 
                     unit-vec)]
    (assoc animal :vx vx :vy vy )))

(defn animal-to-vec [{:keys [x y]}]
  [x y])

(defn desired-location [animal opponent-positions]  ; Split this out, it was getting too complex for "direction"
  (let [my-position (animal-to-vec animal)
        distance-to-target #(len (sub my-position %))
        offsets [(- screen-size) 0 screen-size]     ; you could be aiming to wrap around the screen
        possible-targets (for [[x y] opponent-positions
                               xO offsets
                               yO offsets]
                           [(+ x xO) (+ y yO)])      ; these are all of the locations of the opponents assuming the grid was tiled
                                                    ; this handles the border issue
        ; desired-location (-> state opponents sorted first animal-to-vec)]
        ; sorted #(sort-by distance-to-target %)
  ]
  ((partial apply min-key distance-to-target) possible-targets)))
                           ; min-key is obviously preferable to calling sort and then first
                           ; (not that it was obvious to me when we were coding it)
                           ; but the syntax is horrible.  Anyone got a better technique than "partial apply"?

(defn direction [animal strategy opponents state]   ; moved animal to be the "subject" of the direction command
                                                    ; this is both more in line with oo practice and makes the -> work in think
                                                    ; strictly speaking we're breaking LoD with passing in the current state
                                                    ; but we'd need the whole state if we ever made predators hunt in packs
  (let [opponent-positions (->> state opponents (map animal-to-vec))
        target-point (if (empty? opponent-positions)
                        [(/ screen-size 2) (/ screen-size 2)]  ; this fixes the null reference exception that everyone enjoyed watching ;)
                        (desired-location animal opponent-positions))]
    (move-towards animal target-point strategy)))

(defn think [current-state]
  (let [{:keys [prey predators]} current-state
        new-predators (map #(-> % (direction target :prey current-state) move) predators)]
    (assoc current-state
           :predators new-predators
           :prey (->>                                     ; N.B.  The rule seems to be "use -> for a singular object"
                   prey                                   ; and use ->> for lists.  Arguably the parameters to map and filter are the wrong way around
                   (filter #(surviving? new-predators %))
                   (map #(-> % (direction flee :predators current-state) move))
                   ))))

(defn pulse []
	(let [bounded-screen-size (- screen-size 20)]
	(if (empty? @animals)
		(reset! animals (initial-state))
		(swap! animals think))))
