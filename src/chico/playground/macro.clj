(ns chico.playground.macro)


(defn visualize-fn-call [fn args]
  (let [result (fn args)]
    (println (str "Calling function: " fn " with arguments: " args))
    (println (str "Result of " fn ": " result))
    result))

(defn factorial [n]
  (if (zero? n)
    1
    (* n (visualize-fn-call factorial (dec n)))))

(println (visualize-fn-call factorial 4))
