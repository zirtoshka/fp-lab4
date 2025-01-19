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
                          (:description dialogue))]
        (str description "\n"
             (->> (:transitions dialogue)
                  (map-indexed (fn [i t] (str (inc i) ". " (:text t)))) 
                  (clojure.string/join "\n"))))))) 


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
            (when-let [action (:action matching-transition)]
              (action user-id input))
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




(defdialogue :user-options
  "Привет, {{name}}! Что хотите сделать?"
  {:text "Погода" :next-id :weather :action nil}
  {:text "Шутка" :next-id :joke :action nil}
  {:text "Забыть имя" :next-id :forget-name :action nil}
  {:text "Выйти" :next-id nil :action nil})

(defdialogue :weather
  "Сейчас солнечно и +25°C. Хотите вернуться назад?"
  {:text "Вернуться в меню" :next-id :user-options :action nil})

(defdialogue :joke
  "Почему программисты не бегают? Потому что их нельзя заставить выполнять ненужные циклы."
  {:text "Вернуться в меню" :next-id :user-options :action nil})

(defdialogue :forget-name
  "Вы хотите забыть ваше имя? (Да/Нет)"
  {:text "Да" :next-id :main-menu :action (fn [user-id _]
                                            (set-user-state user-id :name nil))}
  {:text "Нет" :next-id :user-options :action nil})



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
            (recur))))))
)


