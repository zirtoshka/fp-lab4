(ns lapuk4.core-test
  (:require [clojure.test :refer [deftest is]]
            [lapuk4.core :refer [defdialogue get-dialogue get-dialogue-for-user get-random-joke get-user-state process-user-input set-user-state]]))

(deftest test-set-user-state
  (let [user-id :user1]
    (set-user-state user-id :current-dialogue :main-menu)
    (is (= (:current-dialogue (get-user-state user-id)) :main-menu))))

(deftest test-set-and-get-name
  (let [user-id :user1]
    (set-user-state user-id :name "John")
    (is (= (:name (get-user-state user-id)) "John"))))

(deftest test-reset-name
  (let [user-id :user1]
    (set-user-state user-id :name "John")
    (set-user-state user-id :name nil)
    (is (= (:name (get-user-state user-id)) nil))))

(deftest test-get-dialogue-text
  (let [user-id :user1]
    (set-user-state user-id :current-dialogue :main-menu)
    (let [dialogue (get-dialogue-for-user user-id)]
      (is (string? dialogue)))))

(deftest test-dialogue-transition
  (let [user-id :user1]
    (set-user-state user-id :current-dialogue :user-options)
    (let [dialogue (get-dialogue-for-user user-id)]
      (is (clojure.string/includes? dialogue "Что хотите сделать?")))))

(deftest test-process-user-input-valid
  (let [user-id :user1]
    (set-user-state user-id :current-dialogue :main-menu)
    (let [result (process-user-input user-id "1")]
      (is (string? result)))))

(deftest test-process-user-input-invalid
  (let [user-id :user1]
    (set-user-state user-id :current-dialogue :main-menu)
    (let [result (process-user-input user-id "invalid")]
      (is (map? result)))))

(deftest test-defdialogue
  (defdialogue :test-dialogue
    "Test dialogue"
    {:text "Option 1" :next-id :next-dialogue :action nil}
    {:text "Option 2" :next-id :next-dialogue :action nil})

  (let [dialogue (get-dialogue :test-dialogue)]
    (is (= (:description dialogue) "Test dialogue"))
    (is (not-empty (:transitions dialogue)))))

(deftest test-horoscope-selection-valid
  (let [user-id :user1]
    (set-user-state user-id :current-dialogue :goro-dia)
    (let [result (process-user-input user-id "1")]
      (is (string? result)))))

(deftest test-horoscope-selection-invalid
  (let [user-id :user1]
    (set-user-state user-id :current-dialogue :goro-dia)
    (let [result (process-user-input user-id "invalid")]
      (is (contains? result "Попробуйте снова")))))

(deftest test-get-random-joke
  (let [joke (get-random-joke)]
    (is (string? joke))
    (is (not-empty joke))))

(deftest test-exit
  (let [user-id :user1]
    (set-user-state user-id :current-dialogue :main-menu)
    (let [result (process-user-input user-id "2")]
      (is (string? result)))))

