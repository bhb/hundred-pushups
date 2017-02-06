(ns hundred-pushups.android.core
  (:require [reagent.core :as r :refer [atom]]
            [clojure.pprint :as pp]
            [re-frame.core :refer [subscribe dispatch dispatch-sync]]
            [hundred-pushups.events]
            [hundred-pushups.subs]
            [hundred-pushups.core :as core]))

(def ReactNative (js/require "react-native"))
;; From https://github.com/skv-headless/react-native-scrollable-tab-view
;; We can replace with another nativation library at some future point.
;; The correct incantation for the require can be figured out via the directions
;; at http://blog.fikesfarm.com/posts/2015-07-24-using-react-native-components-in-clojurescript.html
;; Note: the React packager is now requiring packages by by internal numerica ID, so this may break in production.
(def ScrollableTabView (js/require "react-native-scrollable-tab-view/index.js"))

(def app-registry (.-AppRegistry ReactNative))
(def linking (.-Linking ReactNative))
(def text (r/adapt-react-class (.-Text ReactNative)))
(def text-input (r/adapt-react-class (.-TextInput ReactNative)))
(def picker (r/adapt-react-class (.-Picker ReactNative)))
(def picker-item (r/adapt-react-class (.-Picker.Item ReactNative)))
(def view (r/adapt-react-class (.-View ReactNative)))
(def image (r/adapt-react-class (.-Image ReactNative)))
(def touchable-highlight (r/adapt-react-class (.-TouchableHighlight ReactNative)))
(def scroll-view (r/adapt-react-class ReactNative.ScrollView))
(def scrollable-tab-view (r/adapt-react-class ScrollableTabView))
(def pushup-form-url "http://www.100pushups.com/perfect-pushups-posture/")

(def styles
  {:tab {:flex 1
            :padding-top 10
            :padding-right 10
            :padding-left 10}
   }
  )

(defn alert [title]
  (.alert (.-Alert ReactNative) title))

(defn get-started []
  [touchable-highlight {:style {:background-color "#999" :padding 10 :border-radius 5}
                        :on-press #(do
                                     (dispatch [:complete-stage :get-started])
                                     (dispatch [:db/save]))}
   [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "Let's get started"]])

(defn learn-pushup-form []
  [view {}
   [text {:style {:text-align "center"}} "Before you start doing pushups, learn optimal pushup technique"]
   [touchable-highlight {:style {:background-color "#999" :padding 10 :border-radius 5}
                        :on-press #(.openURL linking pushup-form-url)}
    [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "Read article about form"]
    ]
   [touchable-highlight {:style {:background-color "#999" :padding 10 :border-radius 5 :margin 10}
                         :on-press #(do
                                      (dispatch [:complete-stage :learn-pushup-form])
                                      (dispatch [:db/save]))}
    [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "OK, I've got it."]]])

;; http://www.100pushups.com/max-pushups-test/
(defn do-pushup-test []
  (let [ui-state (subscribe [:ui-state/get])]
    (fn []
      [view {}
       [text {:style {:text-align "center"}} "Do as many good pushups as you can."]
       [text {:style {:text-align "center"}} "Stop when it takes more than five seconds to do a rep or when you can't do any more reps."]
       [text {} "Reps:"]
       [text-input {:style {:height 40 :border-color "grey" :border-width 1}
                    :default-value (:pushup-reps-text @ui-state)
                    :on-change-text (fn [text]
                                 (dispatch [:ui-state/set [:pushup-reps-text] text])
                                      (dispatch [:db/save]))}]
       [touchable-highlight {:style {:background-color "#999" :padding 10 :border-radius 5 :margin 10}
                             :on-press #(do
                                          (dispatch [:complete-stage :do-pushup-test])
                                          (dispatch [:db/save]))}
        [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "Done!"]]])))


(defn do-plank-test []
  (let [ui-state (subscribe [:ui-state/get])]
    (fn []
      [view {}
       [text {:style {:text-align "center"}} "Hold a plank for as many breaths as you can (not seconds!)"]
       [text {:style {:text-align "center"}} "Stop when your form starts to get bad (e.g. sagging body)"]
       [text {} "Plank breadths:"]
       [text-input {:style {:height 40 :border-color "grey" :border-width 1}
                    :default-value (:plank-reps-text @ui-state)
                    :on-change-text (fn [text]
                                 (dispatch [:ui-state/set [:plank-reps-text] text])
                                      (dispatch [:db/save]))}]
       [touchable-highlight {:style {:background-color "#999" :padding 10 :border-radius 5 :margin 10}
                             :on-press #(do
                                          (dispatch [:append-test
                                                     {:exr/pushup-reps (js/parseInt (:pushup-reps-text @ui-state))
                                                      :exr/plank-reps (js/parseInt (:plank-reps-text @ui-state))
                                                      }
                                                     ])
                                          (dispatch [:ui-state/clear [[:plank-reps-text]
                                                                      [:pushup-reps-text]]])
                                          (dispatch [:complete-stage :do-plank-test])
                                          (dispatch [:db/save]))}
        [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "Done!"]]])))

(defn invalid-stage []
  [view {:style {:flex-direction "column" :margin 40 :align-items "center"}}
   [text {:style {:font-size 20 :font-weight "100" :margin-bottom 10 :text-align "center"}} "Invalid stage!"]
   [text {} "The current stage is:"]
   [text {:style {:font-family "Menlo"}} (pr-str @(subscribe [:stage]))]
   [text {} "DB is:"]
   [text {:style {:font-family "Menlo"}} (pp/write @(subscribe [:db]) :stream nil)]])

(defn show-day []
  [view {:style {:flex-direction "column" :align-items "center"}}
   [text {:style {:font-size 20 :font-weight "100" :margin-bottom 10 :text-align "center"}} "Today's exercise"]
   (let [schedule @(subscribe [:days-exercise])
         circuit (:exr/circuit schedule)]
     [view {}
      (for [x (range (:exr/sets schedule))]
        [view {:key x}
         [text {:style {:font-size 18 :font-weight "600" :margin-top 10}} (str "Set" x)]
         [text {} (str (:exr/pushup-reps circuit) " pushups")]
         [text {} (str "Hold plank for "(:exr/plank-reps circuit) " breaths")]])
      [touchable-highlight {:style {:background-color "#999" :padding 10 :border-radius 5 :margin-top 20}
                            :on-press #(do
                                         (dispatch [:complete-day schedule])
                                         (dispatch [:db/save]))}
       [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "I did it"]]])])

(defn show-header [title, sub-title]
  [view {}
  (when title
    [text {:style {:font-size 40 :font-weight "100" :margin-bottom 10 :text-align "center"}} title])
  [text {:style {:font-size 20 :font-weight "100" :margin-bottom 20 :text-align "center"}} sub-title]]
  )

(defn set-schedule []
  (let [ui-state (subscribe [:ui-state/get])
        white-list (subscribe [:schedule/get-whitelist])]
    (fn []
      [view
       {:style {:flex 1}}
        ;;[text {} @ui-state]
        ;;[text {} white-list]
        [show-header nil, "Create Schedule"]

        (when-not (nil? (:schedule-error @ui-state))
          [text {:style {:color "red" :text-align "center" :font-weight "bold"}} (:schedule-error @ui-state)])

        [touchable-highlight {:style {:background-color "#999" :padding 10 :border-radius 5 :margin-top 20}
                              :on-press #(do
                                           (if (not-any? nil? [(:schedule-day-text @ui-state) (:start-text @ui-state) (:end-text @ui-state)])
                                             (do
                                              (if (and (core/valid-hour-time (:start-text @ui-state)) (core/valid-hour-time (:end-text @ui-state)))
                                                (do
                                                  (dispatch [:save-white-list (:schedule-day-text @ui-state) (:start-text @ui-state) (:end-text @ui-state)])
                                                  (dispatch [:ui-state/set [:schedule-error] nil])
                                                  (dispatch [:ui-state/set [:start-text] nil])
                                                  (dispatch [:ui-state/set [:end-text] nil])
                                                  (dispatch [:db/save]))
                                                (do
                                                  (dispatch [:ui-state/set [:schedule-error] "time format like 9am or 3pm"])
                                                  (dispatch [:db/save]))
                                                ))
                                             (do
                                              (dispatch [:ui-state/set [:schedule-error] "fill in all schedule values"])
                                              (dispatch [:db/save]))
                                             ))}
         [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "Save Schedule"]]

        [picker {:selected-value (:schedule-day-text @ui-state)
                 :on-value-change (fn [item-data]
                                       (dispatch [:ui-state/set [:schedule-day-text] item-data])
                                       (dispatch [:db/save]))}
          [picker-item {:label"Monday" :value "monday"}]
         [picker-item {:label"Tuesday" :value "tuesday"}]
         [picker-item {:label"Wednesday" :value "wednesday"}]
         [picker-item {:label"Thursday" :value "thursday"}]
         [picker-item {:label"Friday" :value "friday"}]
         [picker-item {:label"Saturday" :value "saturday"}]
         [picker-item {:label"Sunday" :value "sunday"}]]

        [text {} "Start:"]
        [text-input {:style {:height 40 :border-color "grey" :border-width 1}
                     :default-value (:start-text @ui-state)
                     :on-change-text (fn [text]
                                       (dispatch [:ui-state/set [:start-text] text])
                                       (dispatch [:db/save]))}]
        [text {} "End:"]
        [text-input {:style {:height 40 :border-color "grey" :border-width 1}
                     :default-value (:end-text @ui-state)
                     :on-change-text (fn [text]
                                       (dispatch [:ui-state/set [:end-text] text])
                                       (dispatch [:db/save]))}]

        ;; TODO input validation
        ;; setup https://github.com/clj-time/clj-time
        ;; use JS plugins https://github.com/xgfe/react-native-datepicker https://www.npmjs.com/package/react-native-date-time-picker

       [view {:style {:flex 1 :padding 20}}
        ;; https://facebook.github.io/react-native/docs/scrollview.html
        ;; Keep in mind that ScrollViews must have a bounded height in order to work, since they contain unbounded-height children into a bounded container (via a scroll interaction).
        ;; Forgetting to transfer {flex: 1} down the view stack can lead to errors here, which the element inspector makes easy to debug.
       [scroll-view {:style {:flex 1}}
        (for [row @white-list]
          [view {:key row}
           [text {:style {:font-size 18 :font-weight "600" :margin-top 10}} (core/format-whitelist-row row)]
           [touchable-highlight {:style {:background-color "#999" :padding 10 :border-radius 5 :margin-top 20}
                                 :on-press #(do
                                              (dispatch [:remove-from-whitelist (first row)])
                                              (dispatch [:db/save]))}
            [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "Remove"]]])]]])))

(defn show-stage [stage]
  [view {:style {:flex-direction "column" :align-items "center"}}
    [show-header "100 Pushup Challenge", "Become a pushup master"]
    [text {:style {:font-size 20 :font-weight "100" :margin-bottom 20 :text-align "center"}} stage]
    (case stage
      :get-started [get-started]
      :learn-pushup-form [learn-pushup-form]
      :do-pushup-test [do-pushup-test]
      :do-plank-test [do-plank-test]
      :show-day [show-day]
      [invalid-stage])])

(defn dev-menu []
  [view {:style {:flex 1}}
   [text {} "DB"]
   [touchable-highlight {:style {:background-color "#999" :padding 10 :border-radius 5 :margin-top 20}
                         :on-press #(do
                                      (dispatch [:db/reset])
                                      (dispatch [:db/save]))}
    [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "Reset DB"]]
   [scroll-view {:style {:padding-top 20}}
    [text {:style {:font-family "Menlo"
                   :background-color "lightgrey"}}
     (pp/write @(subscribe [:db]) :stream nil)]]])

(defn app-root []
  ;; We intentionally only deref the selected tab once, when the component mounts.
  ;; If we updated the component whenever the selected-tab changed, we'd get weird
  ;; behavior because the scrollable-tab-view is trying to control the tab AND
  ;; we're trying to set it manually. The following code allows us to update
  ;; with figwheel without losing our tab position but will also reset to the default
  ;; tab (set in db.cljs) when we reload the app via React Native.
  (let [initial-tab @(subscribe [:selected-tab])]
    (fn []
      [scrollable-tab-view {:style {:margin-top 20 :flex 1}
                            :on-change-tab (fn [evt]
                                             (dispatch [:select-tab (get (js->clj evt) "i")]))
                            :initial-page initial-tab}
       [scroll-view {:style (:tab styles) :tab-label "schedule"}
        [set-schedule]]
       [scroll-view {:style (:tab styles) :tab-label "work out"}
        [show-stage @(subscribe [:stage])]]
       [scroll-view {:style (:tab styles) :tab-label "dev"}
        [dev-menu]]])))

(defn init []
  (dispatch-sync [:db/reset])
  (dispatch-sync [:boot/init])
  (.registerComponent app-registry "HundredPushups" #(r/reactify-component app-root)))
