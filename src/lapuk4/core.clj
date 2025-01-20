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

(defn get-dialogue-for-user [user-id]
  (let [user-state (get-user-state user-id)
        dialogue-id (:current-dialogue user-state)]
    (if (nil? dialogue-id)
      "До новых встреч!"
      (let [dialogue (get-dialogue dialogue-id)
            description (if (:name user-state)
                          (clojure.string/replace (:description dialogue) "{{name}}" (:name user-state))
                          (:description dialogue))
            transitions (:transitions dialogue)
            inp-req (:input-required dialogue)]
        (if inp-req
          (str description)
          (str description "\n"
               (->> transitions
                    (map-indexed (fn [i t] (str (inc i) ". " (:text t))))
                    (clojure.string/join "\n"))))))))



(defmacro defdialogue [name description & transitions]
  (let [last-item (last transitions)
        input-required (if (and (map? last-item) (contains? last-item :input-required))
                         (:input-required last-item)
                         false)
        processed-transitions
        (map (fn [{:keys [text next-id action]}]
               `(->Transition ~text ~next-id ~action))
             (if input-required (butlast transitions) transitions))]
    `(add-dialogue ~name
                   ~description
                   ~(vec processed-transitions)
                   ~input-required)))


(defn process-user-input [user-id input]
  (let [user-state (get-user-state user-id)
        current-dialogue (get-dialogue (:current-dialogue user-state))
        transitions (:transitions current-dialogue)
        input-required (:input-required current-dialogue)]
    (cond
      input-required
      (do
        (when-let [action (:action (first transitions))]
          (action user-id input))
        (when-let [next-id (:next-id (first transitions))]
          (set-user-state user-id :current-dialogue next-id))
        (get-dialogue-for-user user-id))
      (empty? transitions)
      {:error "Нет доступных действий"}

      :else
      (let [input-index (try
                          (Integer/parseInt input)
                          (catch Exception _ nil))
            matching-transition (cond
                                  (and input-index (<= input-index (count transitions)) (> input-index 0))
                                  (nth transitions (dec input-index) nil)
                                  :else
                                  (some #(when (= input (:text %)) %) transitions))]
        (if matching-transition
          (do
            (println matching-transition)
            (when-let [action (:action matching-transition)]
              (action user-id input)
              (println action))
            (set-user-state user-id :current-dialogue (:next-id matching-transition))
            (get-dialogue-for-user user-id))
          (if (nil? (:current-dialogue user-state))
            "До новых встреч!"
            {:error "Неверный ввод"}))))))

(defdialogue :main-menu
  "Добро пожаловать в чат-бот! Что хотите сделать?"
  {:text "Познакомиться" :next-id :introduce :action nil}
  {:text "Завершить" :next-id nil :action nil})


(defdialogue :introduce
  "Как вас зовут?"
  {:text nil :next-id :user-options
   :action (fn [user-id input]
             (println "Полученный ввод:" input)
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
  {:text "Выйти" :next-id nil :action nil})

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




(def zodiac-signs
  [:aries :taurus :gemini :cancer :leo :virgo :libra :scorpio :sagittarius :capricorn :aquarius :pisces])

(defdialogue :goro-dia
  (str "Выберите свой знак зодиака, введя номер:\n"
       (apply str (map #(str (inc %) "." (name (nth zodiac-signs %)) " ") (range (count zodiac-signs)))))
  {:text nil :next-id :user-options
   :action (fn [user-id input]
             (let [input-index (try
                                 (Integer/parseInt input)
                                 (catch Exception _ nil))]
               (if (and input-index (>= input-index 1) (<= input-index (count zodiac-signs)))
                 (let [selected-sign (nth zodiac-signs (dec input-index))] ; Получаем знак по индексу
                   (println (get-horoscope selected-sign))
                   (set-user-state user-id :current-dialogue :user-options))  ; Возвращаем в меню
                 (println "Неверный выбор! Попробуйте снова."))))}
  {:input-required true})




(defn start-chat [user-id]
  (set-user-state user-id :current-dialogue :main-menu)
  (let [dialogue (get-dialogue-for-user user-id)]
    (if (str/blank? dialogue)
      "Ошибка: Не удалось загрузить начальный диалог."
      dialogue))
  (get-dialogue-for-user user-id))



(defn -main [& args]
  (println "Добро пожаловать в чат-бот!")
  (let [user-id :user1]
    (println (start-chat user-id))
    (loop []
      (let [input (read-line)
            result (process-user-input user-id input)]
        (if (string? result)
          (do
            (println result)
            (when (= result "До новых встреч!")
              (System/exit 0))
            (recur))
          (do
            (println (or (:error result) "Ошибка"))
            (recur)))))))


