;;------------------------------------------------------------------------------
;; setup first day of week
;;------------------------------------------------------------------------------
(setq calendar-week-start-day 1)

;;------------------------------------------------------------------------------
;; setup week number
;;------------------------------------------------------------------------------
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-warning-face))

(setq calendar-intermonth-header
      (propertize "НН"
                  'font-lock-face 'font-lock-keyword-face))


;;------------------------------------------------------------------------------
;; setup russian localization
;;------------------------------------------------------------------------------
(setq calendar-day-name-array ["Восресенье" "Понедельник" "Вторник" "Среда" "Четверг" "Пятница" "Суббота"]
      calendar-day-header-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
                                     "Июнь" "Июль" "Август" "Сентябрь"
                                     "Октябрь" "Ноябрь" "Декабрь"])

(provide 'config-calendar)
