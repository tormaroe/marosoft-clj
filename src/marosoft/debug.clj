(ns marosoft.debug
    "Debug repl inspired by The Joy of Clojure 
    (a great book, you should read it!).")

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym))
                 symbols)
            symbols)))

(defn readr [prompt exit-code]
      (let [input (clojure.main/repl-read prompt exit-code)]
        (condp = input 
               ::t1 exit-code
               ::v (local-context)
               input)))

(defn contextual-eval [ctx expr]
      (eval 
        `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
           ~expr)))

(defmacro break 
  "Break execution and enter a debug repl where you may examine
  the local context.
   
  You may specify some optional arguments. Usage:

    (break) ; breaks always
    (break :if condition) ; breaks if condition evaluates to true
    (break :prompt \"debug1\") ; lets you specify the prompt"
  [& {:keys [if prompt] :or {if true, prompt "debug"}}]
  `(when ~if
     (println "--- DEBUG MODE -------------------------------")
     (println "-   (local-context)   display local context")
     (println "-   ::t1              stops debugging")
     (newline)
     (clojure.main/repl
        :prompt #(print (str ~prompt "=> "))
        :read readr
        :eval (partial contextual-eval (local-context)))))
