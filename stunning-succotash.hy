(require hy.contrib.loop)
(import [collections [namedtuple]]
        [rx          [Observable]]
        [rx.subjects [Subject]]
        [blessings   [Terminal]]
        [sys         [exit stdout]]
        [getch       [getch]]
        [operator    [contains add]]
        [functools   [partial]])

(def world
  ["######## "
   "#......# "
   "#......# "
   "#......# "
   "#......# "
   "###..### "
   "  #..#   "
   "  #..#   "
   "  #..####"
   "  #.....#"
   "  #.....#"
   "  #######"])

(def Inscription
  (namedtuple "Inscription"
    (, "x" "y" "sym" "contents")))

(def inscriptions
  (map (fn [props] (apply Inscription props))
    [[1 1 "a" "You see an ant."]]))

(defn index [pred it]
  (let [res (list (filter pred it))]
    (if (empty? res)
      None
      (first res))))

(defn inscriptionat [x y]
  (index
    (fn [i]
      (and (= x (. i x))
           (= y (. i y))))
    inscriptions))

(defn display [str]
  (print str :end ""))

(defn tileat [x y]
  (get (get world y) x))

(defn relpos [origin rel]
  [(+ (get origin 0) (get rel 0))
   (+ (get origin 1) (get rel 1))])

(defmain [&rest args]
  (def term (Terminal))

  (def keys (Subject))

  (def quits (.filter keys (fn [k] (or (= k "q") (= (ord k) 27)))))

  (def movkeys (.filter keys (partial contains "hljknbuy")))

  (def dxs
    (.map movkeys
      (fn [k]
        (if (or (= k "h") (= k "b") (= k "y")) -1
            (or (= k "l") (= k "n") (= k "u")) 1
            0))))

  (def dys
    (.map movkeys
      (fn [k]
        (if (or (= k "k") (= k "u") (= k "y")) -1
            (or (= k "j") (= k "n") (= k "b")) 1
            0))))

  (def moves
    (->
      (.zip dxs dys (fn [x y] [x y]))
      (.start-with [4 4])))

  (def moveresults
    (->
      (.first moves)
      (.map (fn [m] [True m]))
      (.concat (.skip moves 1))
      (.scan
        (fn [cur next]
          (let [adj (relpos (get cur 1) next)]
            (if (= (apply tileat adj) "#")
              [False cur]
              [True adj]))))))

  (def positions
    (->
      (.filter moveresults
        (fn [res] (= True (get res 0))))
      (.map (fn [res] (get res 1)))))

  (def bumps
    (->
      (.filter moveresults
        (fn [res] (= False (get res 0))))
      (.map (fn [res] (get res 1)))))

  (def prevpositions
    (->
      (.start-with positions [None None])
      (.scan
        (fn [cur next]
          [(get cur 1) next]))
      (.skip 2)
      (.map first)))

  (with [(.fullscreen term)]
    (display (.clear term))

    (with [(.location term 0 0)]
      (for [line world] (print line)))

    (.subscribe quits exit)

    (.subscribe prevpositions
      (fn [pos]
        (with [(.location term (get pos 0) (get pos 1))]
          (display (apply tileat pos))
          (.flush stdout))))

    (.subscribe positions
      (fn [pos]
        (with [(.location term (get pos 0) (get pos 1))]
          (display "@")
          (.flush stdout))))

    (loop []
      (.on-next keys (getch))
      (recur))))
