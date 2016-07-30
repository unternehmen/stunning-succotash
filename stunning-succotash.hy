(require hy.contrib.loop)
(import [rx          [Observable]]
        [rx.subjects [Subject]]
        [blessings   [Terminal]]
        [sys         [exit stdout]]
        [getch       [getch]]
        [itertools   [starmap]]
        [operator    [contains add eq]]
        [functools   [partial]]
        [collections [namedtuple]])

(def world
  ["########           #######                                        "
   "#......#       ####.......####                                    "
   "#......#     ##...............#                                   "
   "#......#   ##...##....#####....#                          ######  "
   "#......#  #...##  ####     #.##                          #......# "
   "###..### #..##         ####....###########################.......#"
   "  #..#   #..#         #..........................................#"
   "  #..#   #..#         #....###.###########################.......#"
   "  #..#####..#         #....# #.#  #.....#                #......# "
   "  #.........#         ###### #.####.....#                 ######  "
   "  #.........#                #..........#                         "
   "  ###########                ############                         "])

(def Thing (namedtuple "Thing" "x y sym contents"))

(def things
  (->>
    [[6 10 "0" "A wobbling egg with red spots.  Perhaps dinosaur..."]
     [64 6 "h" "The throne of games.  Made out of recycled @ symbols."]
     [24 8 "P" "Puff bats his eyelashes."]]
    (starmap Thing)
    (list)))

(defn thingat [x y]
  (let [matches? (fn [t] 
                   (and (= (. t x) x)
                        (= (. t y) y)))
        matched (list (filter matches? things))]
    (if (empty? matched)
      None
      (first matched))))

(defn display [str]
  (print str :end ""))

(defn tileat [x y]
  (get (get world y) x))

(defn translate [a b]
  (->>
    (zip a b)
    (starmap add)
    (list)))

(defn lcons [a b] [a b])

(defn successive [o]
  (.zip o (.skip o 1) lcons))

(defn sublimate [o val]
  (.map o (fn [x] val)))

(defmacro fn* [&rest args]
  (let [g (gensym)]
    `(fn [~g]
       (apply (fn ~@args) ~g))))

(defmain [&rest args]
  (def term (Terminal))

  (def keys (Subject))

  (def quits (.filter keys (fn [k] (or (= k "q") (= (ord k) 27)))))

  (def movkeys (.filter keys (partial contains "hljknbuy")))

  (def xmoves
    (.map movkeys
      (fn [k]
        (if (or (= k "h") (= k "b") (= k "y")) -1
            (or (= k "l") (= k "n") (= k "u")) 1
            0))))

  (def ymoves
    (.map movkeys
      (fn [k]
        (if (or (= k "k") (= k "u") (= k "y")) -1
            (or (= k "j") (= k "n") (= k "b")) 1
            0))))

  (def moves
    (->
      (.zip xmoves ymoves (fn [x y] [x y]))
      (.start-with [4 4])))

  (def positions
    (.scan moves
      (fn [cur next]
        (let [adj (translate cur next)]
          (if (and (= (apply tileat adj) ".")
                   (none? (apply thingat adj)))
            adj
            cur)))))

  (def prevpositions
    (->
      (successive positions)
      (.map first)))

  (def bumps
    (->
      (.skip moves 1)
      (.zip (successive positions) lcons)
      (.filter (fn* [_ successives] (apply eq successives)))
      (.map
        (fn* [move successives]
           (->>
             (first successives)
             (translate move))))))

  (def wallbumpmessages
    (->
      (.filter bumps (fn [pos] (= "#" (apply tileat pos))))
      (sublimate "You hit a wall!")))

  (def objectmessages
    (->
      (.filter bumps
        (fn [pos]
          (not (none? (apply thingat pos)))))
      (.map
        (fn [pos]
          (. (apply thingat pos) contents)))))

  (def messages
    (->
      (.merge wallbumpmessages objectmessages)
      (.buffer :buffer-openings keys)
      (.start-with ["Welcome to the hacker's treasure zoo..."])))

  (with [(.fullscreen term)]
    (display (.clear term))

    (with [(.location term 0 1)]
      (for [line world] (print line)))

    ; Put all touchable objects on the screen.
    (for [thing things]
      (with [(.location term (. thing x) (inc (. thing y)))]
        (display (. thing sym))))

    (.subscribe quits exit)

    (.subscribe messages
      (fn [msgs]
        (let [msg (.join " " msgs)]
          (with [(.location term 0 0)] (print (* " " (. term width))))
          (with [(.location term 0 0)] (print msg)))))

    (.subscribe prevpositions
      (fn [pos]
        (with [(.location term (get pos 0) (inc (get pos 1)))]
          (display (apply tileat pos))
          (.flush stdout))))

    (.subscribe positions
      (fn [pos]
        (with [(.location term (get pos 0) (inc (get pos 1)))]
          (display "@")
          (.flush stdout))))

    (loop []
      (.on-next keys (getch))
      (recur))))
