(ns lapuk4.core
   (:gen-class)
   (:require [clojure.string :as str]))


 (def bot-state (atom {:user nil})) 

 (defn reply [message]
   {:type :reply :message message})

 (defn update-state [update-fn]
   {:type :update-state :fn update-fn})

 (defn conditional-reply [condition message]
   {:type :conditional-reply :condition condition :message message})

 (defn nested-dialogue [dialogue]
   {:type :nested :dialogue dialogue})

 (defn run-action [])

  (defn handle-nested-dialogue [dialogue state]
   (println (:prompt dialogue))
   (doseq [response (:response dialogue)]
     (println "-" (:text response)))
   (let [user-input (read-line)
         handler (some #(when (= user-input (:text %)) %)
                       (:responses dialogue))]
     (if handler
       (run-action state (:action handler) user-input)
       "Извините, я не понял ваш запрос.")))

 (defn run-action [state action input]
   (case (:type action)
     :reply (:message action)
     :update-state (do
                     (swap! state (:fn action))
                     nil)
     :conditional-reply (if ((:condition action) @state)
                          (:message action)
                          nil)
     :nested (handle-nested-dialogue (:dialogue action) state)
     nil))



(defn on-message [trigger handler]
  {:trigger trigger :handler handler})


 (defn match-message [handlers message]
   (some #(when (= (:trigger %) message) (:handler %))
         handlers))

;;  (defmacro define-bot [& handlers]
;;    `(fn [state# message#]
;;       (let [handler# (some (fn [h#]
;;                              (when (= (:trigger h#) message#)
;;                                (:handler h#)))
;;                            [~@handlers])]
;;         (if handler#
;;           (handler# state# message#)
;;           "Простите, я не понимаю эту команду."))))

 (defmacro define-bot [& handlers]
   `(fn [state# message#]
      (let [handler# (match-message [~@handlers] message#)]
        (if handler#
          (handler# state# message#)
          "Простите, я не понимаю эту команду."))))

 (defn prompt-user [prompt]
   (println prompt)
   (read-line))

 (defn send-message [bot]
   (let [user-msg (prompt-user "Вы: ")
         bot-response (bot bot-state user-msg)]
     (when bot-response
       (println "Бот: " bot-response))
     (if (= user-msg "выход")
       (println "Сеанс завершен.")
       (recur bot))))

 (on-message "привет"
             (fn [_ _] (reply "Привет! Я чат-бот. Как я могу помочь?")))

 (def bot
   (define-bot
     (on-message "привет"
                 (fn [_ _] (reply "Привет! Я чат-бот. Как я могу помочь?")))


     (on-message "запомни моё имя"
                 (fn [state _]
                   (let [name (prompt-user "Как вас зовут?")]
                     (do
                       (swap! state assoc :user name)
                       (reply (str "Рад познакомиться, " name "!"))))))

     (on-message "пошути"
                 (fn [_ _]
                   (reply "Почему программисты любят зиму? Потому что у них нет багов, только снежинки!")))

     (on-message "угадай число"
                 (fn [_ _]
                   (let [target (rand-int 10)]
                     (loop []
                       (let [guess (Integer. (prompt-user "Угадайте число от 0 до 9:"))]
                         (cond
                           (= guess target) "Поздравляю! Вы угадали!"
                           (< guess target) (do (println "Загаданное число больше.") (recur))
                           :else (do (println "Загаданное число меньше.") (recur))))))))

     (on-message "тест"
                 (fn [_ _]
                   (let [answers [(prompt-user "Ваш любимый цвет? (например, красный)")
                                  (prompt-user "Вы предпочитаете активность или покой? (активность/покой)")
                                  (prompt-user "Какой ваш любимый напиток? (например, чай)")]
                         result "На основе ваших ответов вы больше похожи на... Кроша!"]
                     (reply result))))

     (on-message "погода"
                 (fn [_ _] (reply "Сейчас в Ромашковой долине солнечно и +25°C.")))

     (on-message "выход"
                 (fn [_ _] (reply "До свидания! Возвращайтесь ещё!")))))

 (defn -main []
   (println "Добро пожаловать в чат-бот! Напишите 'выход', чтобы завершить.")
   (send-message bot))


