(ns lapuk4.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defrecord Transition [text next-id action])
(defrecord Dialogue [id description transitions input-required])

(def bot-state (atom {:user nil :current-dialogue :main-menu :name nil}))
(def dialogues (atom {}))

(defn set-user-state [user-id key value]
  (swap! bot-state assoc-in [user-id key] value))

(defn get-user-state [user-id]
  (get @bot-state user-id {:name nil :current-dialogue :main-menu}))

(defn add-dialogue [name description transitions & [input-required]]
  (swap! dialogues assoc name (->Dialogue name description transitions input-required)))

(defn get-dialogue [id]
  (get @dialogues id))

(defn format-description [dialogue user-state]
  (let [description (:description dialogue)]
    (if-let [name (:name user-state)]
      (clojure.string/replace description "{{name}}" name)
      description)))

(defn format-transitions [transitions]
  (->> transitions
       (map-indexed (fn [i t] (str (inc i) ". " (:text t))))
       (clojure.string/join "\n")))

(defn get-dialogue-text [dialogue user-state]
  (let [description (format-description dialogue user-state)
        transitions (:transitions dialogue)
        inp-req (:input-required dialogue)]
    (if inp-req
      description
      (str description "\n" (format-transitions transitions)))))

(defn get-dialogue-for-user [user-id]
  (let [user-state (get-user-state user-id)
        dialogue-id (:current-dialogue user-state)]
    (if (nil? dialogue-id)
      "До новых встреч!"
      (let [dialogue (get-dialogue dialogue-id)]
        (get-dialogue-text dialogue user-state)))))



(defmacro defdialogue [name description & transitions]
  (letfn [(is-input-required [item]
            (and (map? item) (contains? item :input-required)))
          (process-transition [t]
            (let [{:keys [text next-id action]} t]
              `(->Transition ~text ~next-id ~action)))
          (split-transitions [transitions]
            (let [last-item (last transitions)]
              (if (is-input-required last-item)
                [(butlast transitions) (:input-required last-item)]
                [transitions false])))]
    (let [[transitions input-required] (split-transitions transitions)
          processed-transitions (map process-transition transitions)]
      `(add-dialogue ~name
                     ~description
                     ~(vec processed-transitions)
                     ~input-required))))

(defn parse-input [input]
  (try
    (Integer/parseInt input)
    (catch Exception _ nil)))


(defn find-transition [transitions input]
  (let [input-index (parse-input input)]
    (cond
      (and input-index (<= input-index (count transitions)) (> input-index 0))
      (nth transitions (dec input-index))

      :else
      (some #(when (= input (:text %)) %) transitions))))

(defn process-transition [user-id transition input]
  (when-let [action (:action transition)]
    (action user-id input))
  (when-let [next-id (:next-id transition)]
    (set-user-state user-id :current-dialogue next-id)))

(defn process-user-input [user-id input]
  (let [user-state (get-user-state user-id)
        current-dialogue (get-dialogue (:current-dialogue user-state))
        transitions (:transitions current-dialogue)
        input-required (:input-required current-dialogue)] 
    (cond
      input-required
      (let [transition (first transitions)]
        (process-transition user-id transition input)
        (get-dialogue-for-user user-id))

      (empty? transitions)
      {:error "Нет доступных действий"}

      :else
      (if-let [matching-transition (find-transition transitions input)]
        (do
          (process-transition user-id matching-transition input)
          (get-dialogue-for-user user-id))
        (if (nil? (:current-dialogue user-state))
          "До новых встреч!"
          {:error "Неверный ввод"})))))


(defdialogue :main-menu
  "Добро пожаловать в чат-бот! Что хотите сделать?"
  {:text "Познакомиться" :next-id :introduce :action nil}
  {:text "Завершить" :next-id nil :action (fn [user-id _]
                                            (set-user-state user-id :current-dialogue nil))})


(defdialogue :introduce
  "Как вас зовут?"
  {:text nil :next-id :user-options
   :action (fn [user-id input]
             (let [name (if (clojure.string/blank? input)
                          "Неопознанный енот"
                          input)]
               (set-user-state user-id :name name)))}
  {:input-required true})


(def jokes
  ["Почему программисты не бегают? Потому что их нельзя заставить выполнять ненужные циклы."
   "Почему не стоит обсуждать с программистами погоду? Потому что они всегда будут искать баги."
   "Как программисты делают кофе? Они делают его через команду brew."
   "Почему программисты так редко влюбляются? Потому что они всегда проверяют, нет ли ошибок в их коде."
   "Что говорят программисты, когда что-то ломается? 'Это не баг, это фича!'"])

(defn get-random-joke []
  (rand-nth jokes))

(def horoscopes
  {:aries   "Сегодня день, когда ваши усилия начнут приносить результаты. Будьте смелыми!"
   :taurus  "Отличный день для принятия важных решений. Не бойтесь рисковать!"
   :gemini  "Сегодня вы будете полны энергии, так что воспользуйтесь этим для работы!"
   :cancer  "Может возникнуть некоторое беспокойство, но вам удастся преодолеть все трудности."
   :leo     "Сегодняшний день будет удачным для общения и новых начинаний."
   :virgo   "Время для размышлений и планирования. Сегодня ваши идеи будут особенно ясны."
   :libra   "Вы будете чувствовать гармонию в отношениях с другими людьми. Постарайтесь сохранить этот баланс."
   :scorpio "Сегодня важно доверять своей интуиции. Не бойтесь делать неожиданные шаги."
   :sagittarius "Ваши идеи сегодня могут оказаться очень удачными. Действуйте решительно."
   :capricorn "Наступает время для работы и преодоления трудностей. Вы справитесь!"
   :aquarius "Сегодня отличный день для новых начинаний, вы полны энергии."
   :pisces  "Время для отдыха и восстановления. Постарайтесь не перенапрягаться."})

(defn get-horoscope [zodiac-sign]
  (get horoscopes zodiac-sign "Не удалось получить гороскоп для этого знака."))



(defdialogue :user-options
  "Dear, {{name}}! Что хотите сделать?"
  {:text "Новости" :next-id :news-topic :action nil}
  {:text "Гороскоп" :next-id :goro-dia :action nil}
  {:text "Шутка" :next-id :user-options :action (fn [_ _]
                                                  (println (get-random-joke)))}
  {:text "Забыть имя" :next-id :forget-name :action nil}
  {:text "Выйти" :next-id nil :action (fn [user-id _]
                                        (let [name ((get-user-state user-id) :name)]
                                          (println (str name ", пока-пока!"))
                                          (set-user-state user-id :current-dialogue nil)))})


(defdialogue :news-topic
  "Какие новости вас интересуют?"
  {:text "Спорт" :next-id :sport :action nil}
  {:text "Политика" :next-id :politic :action nil}
  {:text "Образование" :next-id :education :action nil}
  {:text "Вернуться в меню" :next-id :user-options :action nil})

(defdialogue :sport
  "А сейчас к новостям спорта..."
  {:text "Вернуться" :next-id :news-topic :action nil}
  {:text "Вернуться в меню" :next-id :user-options :action nil})

(defdialogue :politic
  "А сейчас к новостям ..."
  {:text "Вернуться" :next-id :news-topic :action nil}
  {:text "Вернуться в меню" :next-id :user-options :action nil})

(defdialogue :education
  "А сейчас к новостям ..."
  {:text "Вернуться" :next-id :news-topic :action nil}
  {:text "Вернуться в меню" :next-id :user-options :action nil})


(defdialogue :forget-name
  "Вы хотите забыть ваше имя?"
  {:text "Да" :next-id :main-menu :action (fn [user-id _]
                                            (set-user-state user-id :name nil))}
  {:text "Нет" :next-id :user-options :action nil})


(defn valid-input? [input max]
  (let [parsed (parse-input input)]
    (and parsed (>= parsed 1) (<= parsed max))))

(def zodiac-signs
  [:aries :taurus :gemini :cancer :leo :virgo :libra :scorpio :sagittarius :capricorn :aquarius :pisces])

(defn handle-horoscope-selection [_ input]
  (let [input-index (parse-input input)]
    (if (valid-input? input (count zodiac-signs))
      (let [selected-sign (nth zodiac-signs (dec input-index))]
        (println (get-horoscope selected-sign)))
      (println "Неверный выбор! Попробуйте снова."))))


(defdialogue :goro-dia
  (str "Выберите свой знак зодиака, введя номер:\n"
       (apply str (map #(str (inc %) ". " (name (nth zodiac-signs %)) " ") (range (count zodiac-signs)))))
  {:text nil :next-id :user-options
   :action (fn [user-id input]
             (handle-horoscope-selection user-id input))}
  {:input-required true})




(defn start-chat [user-id]
  (set-user-state user-id :current-dialogue :main-menu)
  (let [dialogue (get-dialogue-for-user user-id)]
    (if (str/blank? dialogue)
      "Ошибка: Не удалось загрузить начальный диалог."
      dialogue)))

(defn handle-result [result]
  (if (string? result)
    (if (= result "До новых встреч!")
      (do (println result) true)
      (do (println result) false))
    (do (println (or (:error result) "Ошибка")) false)))


(defn process-chat-loop [user-id]
  (loop []
    (let [input (read-line)
          result (process-user-input user-id input)
          exit? (handle-result result)]
      (if exit?
        (System/exit 0)
        (recur)))))

(defn -main [& _]
  (let [user-id :user1
        start-message (start-chat user-id)]
    (println start-message)   
    (process-chat-loop user-id))) 


