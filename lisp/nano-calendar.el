;;; nano-calendar.el --- Emacs calendar settings for active locale -*- lexical-binding: t; -*-

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023-2024 - N Λ N O developers

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'calendar)
(require 'nano-modeline)
(require 'russian-holidays)

;; Set Russian names, abbrevs and holidays
(if (string= (getenv "LANG") "ru_RU.UTF-8")
    (setq calendar-week-start-day 1           ; Start week from Monday
          calendar-date-style 'european       ; Use the DD/MM/YYYY format for the diary dates

          calendar-day-name-array     ["Воскресенье" "Понедельник" "Вторник" "Среда"
                                       "Четверг" "Пятница" "Суббота"]
          calendar-day-header-array   ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
          calendar-day-abbrev-array   ["Вск" "Пнд" "Втр" "Сре" "Чтв" "Птн" "Суб"]
          calendar-month-name-array   ["Январь" "Февраль" "Март" "Апрель" "Май"
                                       "Июнь" "Июль" "Август" "Сентябрь"
                                       "Октябрь" "Ноябрь" "Декабрь"]
          calendar-month-abbrev-array ["Янв" "Фев" "Мар" "Апр" "Май" "Июн" "Июл" "Авг" "Сен" "Окт" "Ноя" "Дек"]

          calendar-holidays russian-holidays))

;; Highlight holidays by default
(setq calendar-mark-holidays-flag t)

(add-hook 'calendar-mode-hook #'(lambda () (nano-modeline nano-modeline-format-calendar)))

(provide 'nano-calendar)
;;; nano-calendar.el ends here
