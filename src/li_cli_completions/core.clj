(ns li-cli-completions.core
  (:require [babashka.process :as proc]
            [clojure.string :as str]
            [lambdaisland.cli :as cli]))

(defn args-and-word
  "Given the line and cursor index, returns the args up to the cursor and the
  current word to be completed as a tuple [args word]."
  [line idx]
  (let [l (subs line 0 idx)
        ;; The first word is the executable.
        words (rest (proc/tokenize l))]
    (if (or (re-find #"\s$" l) (= "" l))
      [words ""]
      (if-some [[opt word]
                (some->> (last words)
                         (re-find #"^(--[^-^=]+)=(.*)$")
                         rest
                         not-empty)]
        [(concat (butlast words) [opt]) word]
        [(or (butlast words) ()) (or (last words) "")]))))

(defn get-completions [cmdspec args word]
  (loop [{:keys [flags] :as cmdspec} cmdspec
         [arg & more-args :as args] args
         ctx {::cli/command []}]
    (let [{:keys [command commands flagmap] :as new-cmdspec}
          (cli/add-processed-flags cmdspec flags)

          cmd-map (into {} (cli/prepare-cmdpairs commands))]
      (cond (empty? args)
            ;; TODO:  This should look at what word starts with first,
            ;; then fall back to this.
            (if commands
              (keys cmd-map)
              (filter cli/long? flagmap))

            (or (cli/long? arg) (cli/short? arg))
            ;; TODO: Handle flags like --flag=arg
            (if-some [{argnames :args
                       argcnt :argcnt
                       completions :completions
                       :as flag}
                      (get flagmap arg)]
              (if (zero? argcnt)
                (recur new-cmdspec more-args ctx)
                (if (<= (count more-args) argcnt)
                  (cond (fn? completions) (completions)

                        (map? completions)
                        (let [current-arg (->> argnames
                                               (drop (count more-args))
                                               first)]
                          ((get completions current-arg)))

                        :else (seq completions))
                  (recur new-cmdspec (drop argcnt more-args) ctx)))
              ;; This should probably just give up if we're in :strict? mode.
              (recur new-cmdspec more-args ctx))

            commands
            (when-some [next-cmd* (cmd-map arg)]
              (let [{:keys [argnames completions] :as next-cmd}
                    (cli/to-cmdspec next-cmd*)

                    argcnt (count argnames)]
                (if (zero? argcnt)
                  (recur (merge (dissoc new-cmdspec :commands :flags)
                                next-cmd)
                         more-args
                         (update ctx ::cli/command conj arg))
                  (if (<= (count more-args) argcnt)
                    (cond (fn? completions) (completions)

                          (map? completions)
                          (let [current-arg (->> argnames
                                                 (drop (count more-args))
                                                 first)]
                            ((get completions current-arg)))

                          :else (seq completions))
                    (recur new-cmdspec (drop argcnt more-args) ctx)))))

            ;; If we get to this point, I don't think there's anything
            ;; left to do, because positional args are handled in
            ;; `commands` above, and clis without subcommands don't
            ;; seem to be able to specify names for positional commands(?).
            command []))))

(comment

  [(cli/prepare-cmdpairs ["foo <arg1> <arg2>" {}])
   (cli/prepare-cmdpairs ["foo" #'get-completions])]


  )

(def log-file "")

(defn log [k m]
  (when false
    (spit log-file (str (pr-str [k m]) "\n") :append true)))

(defn print-bash-completions [completions]
  (run! println (cons "next" completions)))

(defn filter-prefix [prefix completions]
  (cond->> completions
    (not (str/blank? prefix))
    (filter #(and (-> (cond-> % (map? %) :candidate)
                      (str/starts-with? prefix))
                  (not= % prefix)))))

(defn print-completions [{:keys [shell]} cmdspec]
  (case shell
    "bash"
    (let [line (System/getenv "COMP_LINE")
          point (parse-long (System/getenv "COMP_POINT"))
          _ (log :input {:line line, :point point})
          [args word] (args-and-word line point)
          completions (try (->> (get-completions cmdspec args word)
                                (filter-prefix word))
                           (catch Throwable _
                             ;; TODO: log error
                             ))]
      (log :completions {:completions completions})
      (print-bash-completions completions))))

(defn bash-fn-name [command-name]
  ;; Dunno what's actually allowed, in practice and theoretically in POSIX
  ;; or whatever. We just use munge and hope that's good enough ¯\_(ツ)_/¯.
  ;; https://stackoverflow.com/questions/28114999/what-are-the-rules-for-valid-identifiers-e-g-functions-vars-etc-in-bash
  (str "_" (munge command-name) "_completions"))

(defn bash-script [command-name single-command?]
  (let [fn-name (bash-fn-name command-name)
        completion-args (if single-command?
                          "--shell-completions-complete=bash"
                          "shell-completions complete bash")
        completions-command "${COMP_WORDS[0]}"]
    (str "function " fn-name "()
{
    export COMP_LINE=${COMP_LINE}
    export COMP_POINT=$COMP_POINT

    RESPONSE=($(" completions-command " " completion-args "))

    # The first line is a directive.
    # Currently only [next | continue]
    if [ ${RESPONSE[0]} = 'next' ]; then
        compopt +o nospace
    fi

    unset RESPONSE[0]

    COMPREPLY=(${RESPONSE[@]})
}
complete -o nospace -F " fn-name " " command-name)))

(defn print-script
  [{:keys [shell-completions-command shell single-command]}]
  (println
   (case shell
     "bash" (bash-script shell-completions-command single-command))))

(defn ensure-flat [x]
  (cond->> x (or (map? x) (nil? x)) (into [] cat)))

(defn add-completions [cmdspec]
  (let [{command-name :name, :as cspec} (cli/to-cmdspec cmdspec)
        script-flags ["--shell-completions-command=<command>"
                      {:doc "The name of this executable."
                       :default command-name}]]
    (if (:command cspec)
      (update cspec :flags
              #(into (ensure-flat %)
                     (concat
                      ["--shell-completions-script=<shell>"
                       {:doc "Print the script for shell completions."
                        :key :shell
                        :middleware (constantly
                                     (fn [opts]
                                       (print-script
                                        (assoc opts :single-ommand true))))}

                       "--shell-completions-complete=<shell>"
                       {:key :shell
                        :middleware (constantly
                                     (fn [opts]
                                       (print-completions opts cspec)))}]
                      script-flags)))

      (update cspec :commands
              #(into (ensure-flat %)
                     ["shell-completions"
                      {:doc "Shell completion commands"
                       :commands ["script <shell>"
                                  {:command print-script
                                   :doc "Print the script for shell completions."
                                   :flags script-flags}

                                  "complete <shell>"
                                  {:command (fn [opts]
                                              (print-completions opts cspec))}]}])))))

(comment

  ["With bash the completion script can be installed"
   "permanently by creating a file in /etc/bash_completion.d"
   "For example: (Change <command> to the name of your command.)"
   ""
   (str "$ <command> " command " script bash | sudo tee /etc/bash_completion.d/<command> >/dev/null")
   ""
   "or"
   (str "$ echo 'source <(<command> " command " script bash)' >> ~/.bashrc")]


  (defn out-to-file-mw [k]
    (fn [cmd]
      (fn [opts]
        (let [file (get opts k)]
          (binding [*out* (java.io.FileWriter. file)]
            (cmd opts))))))

  ((requiring-resolve `cli/to-cmdspec)
   (requiring-resolve `cli/to-cmdspec))

  (defn late-bound [sym]
    (fn [x]
      ((requiring-resolve sym) x)))

  )
