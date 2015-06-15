;;; chinese-calendar.el --- calendar for chinese

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Charles Wang  for the original version  
;;         Milton Wu(wulei) for the current version (miltonwulei@163.com)
;; Keywords: calendar, i18n

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; installation:
;;  put the following line in your .emacs
;;
;;                (add-to-path 'load-path "path/where/to/chinese-calendar.el"
;;                (require 'chinese-calendar)
;;; Code:


(require 'calendar)
(require 'cal-china)
(require 'cal-move)
(require 'holidays)
(require 'diary-lib)
(require 'solar)

(defvar displayed-month)
(defvar displayed-year)
(defun my-calendar-remapkey())
;(add-hook 'calendar-load-hook 'my-calendar-remapkey)

(defun my-calendar-move-hook ()
  (my-calender-set-mode-line))
(add-hook 'calendar-move-hook 'my-calendar-move-hook)

(defun my-calendar-init-windows-hook()
  (my-calender-set-mode-line))
(add-hook 'initial-calendar-window-hook 'my-calendar-init-windows-hook)

;;始终让当前的月份居中
(setq calendar-offset 0)
(setq chinese-calendar-celestial-stem
  ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq chinese-calendar-terrestrial-branch
  ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(defvar chinese-shuxiang-name
  ["鼠" "牛" "虎" "兔" "龙" "蛇" "马" "羊" "猴" "鸡" "狗" "猪"])
(defvar chinese-number ["一" "二" "三" "四" "五" "六" "七" "八" "九" "十"])
(defvar chinese-calendar-jiqi-name
  ["小寒" "大寒" "立春" "雨水" "惊蛰" "春分"
   "清明" "谷雨" "立夏" "小满" "芒种" "夏至"
   "小暑" "大暑" "立秋" "处暑" "白露" "秋分"
   "寒露" "霜降" "立冬" "小雪" "大雪" "冬至"]
)
;;store the current year's jieqi list
(defvar chinese-calendar-current-year-jieqi-list nil)
(defvar my-chinese-month-name 
  [
   "正月" "二月" "三月" "四月" "五月" "六月" 
   "七月" "八月" "九月" "十月" "冬月" "腊月"])
(defvar my-chinese-day-name 
      [
       "初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十" 
       "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九" "二十"
       "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
       "卅一" "卅二" "卅三" "卅四" "卅五" "卅六" "卅七" "卅八" "卅九" "卅十"
       ])
(setq calendar-day-abbrev-array  
  ["Sun日" "Mon一" "Tue二" "Wed三" "Thu四" "Fri五" "Sat六" ])
(setq calendar-day-name-array calendar-day-abbrev-array)
;(setq calendar-day-name-array
;      ["星期日" "星期一" "星期二" "星期三" "星期四" "星期五"  "星期六" ])
;(setq calendar-day-abbrev-array      ["Su" "Mo" "Tu" "We" "Th" "Fr" "Sa" ])
;如果是在用汉字表示星期的话需要把汉字调整到和英文用等宽，或者使用fixdays等宽字体
;否则会出现错位的现象,windows下用宋体12，英文courier new 10或中文16英文13

;;定义中国星期的显示格式
(defun my-chinese-day-name (date)
  (let ((day  (calendar-day-of-week date)))
    (concat "星期" 
            (if (eq day 0)
                "日"
              (aref chinese-number (1- day))))))
;;定义中国日期的显示格式
(defun my-calendar-display-form (date)
  (let* ((dayname (my-chinese-day-name date))
         (day   (extract-calendar-day date))
         (month (extract-calendar-month date))
         (year  (extract-calendar-year date)))
    (format "%4d年%2d月%2d日 %s" year month day dayname)))
;;

;;定义日记模式中识别每个条目开始的日期信息
(defvar chinese-date-diary-pattern
;美式格式 
  '((month "/" day "[^/0-9]")
      (month "/" day "/" year "[^0-9]")
      (monthname " *" day "[^,0-9]")
      (monthname " *" day ", *" year "[^0-9]")
      (dayname "\\W")
;中国格式
      (year "年[ ]*" month "月[ ]*" day "日[^/0-9]星期[一二
三四五六日]")
;英式格式
;      (day "/" month "[^/0-9]")
;      (day "/" month "/" year "[^0-9]")
;      (backup day " *" monthname "\\W+\\<[^*0-9]")
;      (day " *" monthname " *" year "[^0-9]")
;      (dayname "\\W")
      )
)
(setq calendar-date-display-form '((my-calendar-display-form date)))
(setq diary-date-forms chinese-date-diary-pattern)

;;定义节气的识别函数
(defun chinese-calendar-jieqi-p (month day year)
"If the date is jieqi return t,else return nil"
 ;;if the jieqi-list is empty or the jieqi-list is not for this year
 ;;then create the jieqi-list for the year
 (if (or (equal chinese-calendar-current-year-jieqi-list nil)
         (not
          (equal (car chinese-calendar-current-year-jieqi-list) year)))
     (chinese-calendar-create-jieqi-list year))
 (if (member (list month day) chinese-calendar-current-year-jieqi-list)
     t
     nil)
)

(defun chinese-calendar-get-jieqi-name (month day)
"Get the chinese jieqi name "
  (let ((name-index (+ (* 2 (1- month)) (/ day 15) )))
       (aref chinese-calendar-jiqi-name name-index)
  ))

;;24节气的计算方法是，
;;从冬至开始，地球围绕太阳每转动15度的那一刻就是一个节日
(defun chinese-calendar-next-jieqi-date (d)
"Return the next Jieqi after or on the absolute date d,
the Return data form is absolute date"
   (solar-date-next-longitude d 15))

(defun chinese-calendar-create-jieqi-list (year)
" Create the year's jieqi ,store it in the variable 
chinese-calendar-current-year-jieqi.
The format is (year (month day) (month day) ... (month day))
"
  (if (not (equal chinese-calendar-current-year-jieqi-list nil))
      (setq chinese-calendar-current-year-jieqi-list nil))
(let* (
       (start-date 
         (calendar-astro-from-absolute
         (calendar-absolute-from-gregorian (list 1 1 year))))
       (i 0)
       jieqi-date temp-date
       )
  (setq chinese-calendar-current-year-jieqi-list (list year))      
  (while (< i 24)
    (setq i (1+ i))
    (setq temp-date  (chinese-calendar-next-jieqi-date start-date))
    (setq jieqi-date 
          (calendar-gregorian-from-absolute  
           (floor (calendar-absolute-from-astro temp-date))))
    (setq chinese-calendar-current-year-jieqi-list
          (cons (list (car jieqi-date) (car (cdr jieqi-date)))
          chinese-calendar-current-year-jieqi-list))
    (setq start-date (1+ temp-date)))

  (setq chinese-calendar-current-year-jieqi-list
        (nreverse chinese-calendar-current-year-jieqi-list))
  )
)

;;计算汉字符数目的函数
(defun my-count-chinese-character (string)
  (length (remq nil
                (mapcar 'multibyte-string-p 
                        (mapcar 'char-to-string string)))))



;;将阳历日期换算成阴历
(defun solar-day-to-lunar-day (date)
 (let* ((a-date (calendar-absolute-from-gregorian date));;绝对日期
         (c-date (calendar-chinese-from-absolute a-date));;阴历日期
        (day (car (cdr (cdr (cdr c-date)))))
        )
      (format "%s" (aref my-chinese-day-name (1- day))
      )
 )
)

(defun my-chinese-calendar-get-date-name(date)
"If the date is a jieqi return it's jieqi name
else return it's chinese name"
(let* (
       (a-date (calendar-absolute-from-gregorian date));;绝对日期
       (c-date (calendar-chinese-from-absolute a-date));;阴历日期
       (c-day (car (cdr (cdr (cdr c-date)))))
       (month (car date))
       (day (car (cdr date)))
       (year (car (cdr (cdr date))))
      )
  (if (chinese-calendar-jieqi-p month day year)
      (chinese-calendar-get-jieqi-name month day)
    (if (equal c-day 1)
        (aref my-chinese-month-name (1- (floor (nth 2 c-date))))
    (format "%s" (aref my-chinese-day-name (1- c-day))))))
)

(defun my-chinese-month-string (date)
"chinese month name string ,displayed in the calendar buffer"
  (let* ((a-date (calendar-absolute-from-gregorian date));;绝对日期
         (c-date (calendar-chinese-from-absolute a-date));;阴历日期
         (cycle (car c-date));;在一甲子周期中的位置
         (year (car (cdr c-date)))
         (shuxiang (mod  year 12) );;属相的位置
         (month (car (cdr (cdr c-date))))
         (day (car (cdr (cdr (cdr c-date)))))
         (this-month (calendar-absolute-from-chinese
                      (list cycle year month 1)))
         (next-month (calendar-absolute-from-chinese
                      (list (if (= year 60) (1+ cycle) cycle)
                            (if (= (floor month) 12) (1+ year) year)
                            (calendar-mod (1+ (floor month)) 12)
                            1)))
         (m-cycle (% (+ (* year 5) (floor month)) 60)))
    (format "农历%s年%s%s       %s年      "
            (calendar-chinese-sexagesimal-name year)
            (if (not (integerp month));; .5格式表示是闰月
                "闰"
              (if (< 30 (- next-month this-month))
                  ""
                ""))
            (aref my-chinese-month-name (1- (floor month)))
            (calendar-chinese-shuxiang-name (mod (1- shuxiang) 12))
            
           )
            ))

(defun my-chinese-date-string (date)
  (let* ((a-date (calendar-absolute-from-gregorian date));;绝对日期
         (c-date (calendar-chinese-from-absolute a-date));;阴历日期
         (cycle (car c-date));;在一甲子周期中的位置
         (year (car (cdr c-date)))
         (month (car (cdr (cdr c-date))))
         (day (car (cdr (cdr (cdr c-date)))))
         (this-month (calendar-absolute-from-chinese
                      (list cycle year month 1)))
         (next-month (calendar-absolute-from-chinese
                      (list (if (= year 60) (1+ cycle) cycle)
                            (if (= (floor month) 12) (1+ year) year)
                            (calendar-mod (1+ (floor month)) 12)
                            1)))
         (m-cycle (% (+ (* year 5) (floor month)) 60)))
    (format "农历%s年%s%s%s"
            ;;cycle
            ;;year 
            (calendar-chinese-sexagesimal-name year)
            (if (not (integerp month))
                "闰"
              (if (< 30 (- next-month this-month))
                  ""
                ""))
            (aref my-chinese-month-name (1- (floor month)))
            (aref my-chinese-day-name (1- day))
            )))
;;设置日历显示模式下的状态栏
(defun my-calender-set-mode-line ()
  (let* ((date (calendar-cursor-to-date))
         (s1 (calendar-date-string date t))
         (s2 (my-chinese-date-string date))
         (s3 (my-calendar-cursor-holidays))
         (x (list s1 s2 s3))
         (y (make-string (apply '+  (mapcar 'my-count-chinese-character x )) ? )))
    (progn
      (setq calendar-mode-line-format 
            (append x (list y)))
      (update-calendar-mode-line)
      (force-mode-line-update))))
    
;;; the following function is copied from calendar.el or cal-china.el
;;; because they don't conform with the chinese traditional presentation.

(defun calendar-chinese-sexagesimal-name (n)
  "The N-th name of the Chinese sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  (format "%s%s"
          (aref chinese-calendar-celestial-stem (% (1- n) 10))
          (aref chinese-calendar-terrestrial-branch (% (1- n) 12))))

(defun calendar-chinese-shuxiang-name (n)
 " The N-th shuxiang name of the chinese year."
 (format "%s" (aref chinese-shuxiang-name n) )
)

;;;;在mode line 显示光标当前日期的节日名称，如果该日是节日的话
(defun my-calendar-cursor-holidays ()
  "set mode line , holidays for the date specified by the cursor in the calendar window.
  return today's hoilday name string if today is a holiday ,otherwise return 2 space "
  (let* (
          (date (calendar-cursor-to-date t))
        ; (date-string (calendar-date-string date))
         (holiday-list (check-calendar-holidays date))
         (holiday-string (mapconcat 'identity holiday-list "; ")))
           ( format "  %s" holiday-string)
         )
    )

(defun generate-calendar-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted in the buffer starting at the line on which point
is currently located, but indented INDENT spaces.  The indentation is done
from the first character on the line and does not disturb the first INDENT
characters on the line."
  (let* ((blank-days;; at start of month
          (mod
           (- (calendar-day-of-week (list month 1 year))
              calendar-week-start-day)
           7))
         (last (calendar-last-day-of-month month year))

 )
   (goto-char (point-min))
   (calendar-insert-indented
    (calendar-string-spread
     (list (format "    %4d年%2d月    %s" year month 
         (my-chinese-month-string (list month 1 year)))) ?  36)
     indent t)
   (calendar-insert-indented "" indent);; Go to proper spot
;;插入星期标头，
   (calendar-for-loop i from 0 to 6 do
      (insert (calendar-day-name (mod (+ calendar-week-start-day i) 7)
                                 () t))
      (insert "   "))
   (calendar-insert-indented "" 0 t);; Force onto following line
   (calendar-insert-indented "" indent);; Go to proper spot
   ;; Add blank days before the first of the month
   (calendar-for-loop i from 1 to blank-days do (insert "        "));;8个空格
   ;; Put in the days of the month
   (calendar-for-loop i from 1 to last do
      (insert (format "%2d%s  " i 
               (my-chinese-calendar-get-date-name (list month i year))))
      (add-text-properties
       (- (point) 3) (1- (point))
       '(mouse-face highlight
         help-echo "mouse-2: menu of operations for this date"))
      (and (zerop (mod (+ i blank-days) 7))
           (/= i last)
           (calendar-insert-indented "" 0 t)    ;; Force onto following line
           (calendar-insert-indented "" indent)))));; Go to proper spot


;;重新定义显示日历的窗口，一次只显示一个月的日历
(defun generate-calendar (month year)
  "Generate a one-month Gregorian calendar centered around MONTH, YEAR."
; A negative YEAR is interpreted as BC; -1 being 1 BC, and so on.
; Note that while calendars for years BC could be displayed as it
; stands, almost all other calendar functions (eg holidays) would 
; at best have unpredictable results for such dates.
  (if (< (+ month (* 12 (1- year))) 1)
      (error "Months before January, 1 AD are not available"))
  (setq displayed-month month
        displayed-year year)
  (erase-buffer)
  ;;只产生一个月的月历
  (generate-calendar-month month year 20)
)

;;重定义窗口显示函数
;;防止auto-fill mode justify this window
(defun generate-calendar-window (&optional mon yr)
  "Generate the calendar window for the current date.
Or, for optional MON, YR."
  (let* ((buffer-read-only nil)
;;;don not permit auto-fill mode to justify this buffer
;; default-justification or fill-column automatically became local variable
;; (default-justification nil) 
;;(this sentense or the next has same result, choose one 
         (fill-column 80)
         (today (calendar-current-date))
         (month (extract-calendar-month today))
         (day (extract-calendar-day today))
         (year (extract-calendar-year today))
         (today-visible
          (or (not mon)
              (let ((offset (calendar-interval mon yr month year)))
                (and (<= offset 1) (>= offset -1)))))
         (day-in-week (calendar-day-of-week today)))
    (update-calendar-mode-line)
    (if mon
        (generate-calendar mon yr)
        (generate-calendar month year))
    (calendar-cursor-to-visible-date
     (if today-visible today (list displayed-month 1 displayed-year)))
    (set-buffer-modified-p nil)
    (if (or (one-window-p t) (/= (frame-width) (window-width)))
        ;; Don't mess with the window size, but ensure that the first
        ;; line is fully visible
        (set-window-vscroll nil 0)
      ;; Adjust the window to exactly fit the displayed calendar
      (fit-window-to-buffer))
    (sit-for 0)
    (if (and (boundp 'font-lock-mode)
             font-lock-mode)
        (font-lock-fontify-buffer))
    (and mark-holidays-in-calendar
;;;         (calendar-date-is-legal-p today) ; useful for BC dates
         (mark-calendar-holidays)
         (sit-for 0))
    (unwind-protect
        (if mark-diary-entries-in-calendar (mark-diary-entries))
      (if today-visible
          (run-hooks 'today-visible-calendar-hook)
        (run-hooks 'today-invisible-calendar-hook)))))


;;重新定义 calendar-date-is-visible-p函数，
;;该函数在calendar的很多函数中作为判断该日期是否显示在
;;窗口中,以便在需要的时候重绘窗口
(defun calendar-date-is-visible-p (date)
  "Return t if DATE is legal and is visible in the calendar window."
  (let ((gap (calendar-interval
              displayed-month displayed-year
              (extract-calendar-month date) (extract-calendar-year date))))
    (and (calendar-date-is-legal-p date) (> 1 gap) (< -1 gap))))

;;重定义在日期之间移动的基本函数
(defun calendar-cursor-to-visible-date (date)
  "Move the cursor to DATE that is on the screen."
  (let* ((month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (first-of-month-weekday (calendar-day-of-week (list month 1 year))))
    (goto-line (+ 3 ;;日期从第三行开始
                  (/ (+ day  -1
                        (mod
                         (- (calendar-day-of-week (list month 1 year))
                            calendar-week-start-day)
                         7))
                     7)))
    (move-to-column (+ 20
                       (* 8 (mod
                             (- (calendar-day-of-week date)
                                calendar-week-start-day)
                             7)))))
   ; (calendar-cursor-to-nearest-date)
)

;;重定义让光标在日期之间准确定位的函数
(defun calendar-cursor-to-nearest-date ()
  "Move the cursor to the closest date.
The position of the cursor is unchanged if it is already on a date.
Returns the list (month day year) giving the cursor position."
  (let ((date (calendar-cursor-to-date))
        (column (current-column)))
    (if date
        date
      (if (> 3 (count-lines (point-min) (point)));;日期从第三行开始
          (progn
            (goto-line 3)
            (move-to-column column)))
      (if (not (looking-at "[0-9]"))
          (if (and (not (looking-at " *$"));;非行尾
                   (< column 21))
              (progn
                (re-search-forward "[0-9]" nil t)
                (backward-char 1)
              )
            (re-search-backward "[0-9]" nil t)))
      (calendar-cursor-to-date))))


;;将光标所在的日期转换成date对象
(defvar calendar-starred-day)
(defun calendar-cursor-to-date (&optional error)
  "Return a list (month day year) of current cursor position.
If cursor is not on a specific date, signals an error if optional parameter
ERROR is t, otherwise just returns nil."
  (let* (
         (month  displayed-month)
         (year   displayed-year))
    (if (and (looking-at "[ 0-9]?[0-9][^0-9]")
             (< 2 (count-lines (point-min) (point))))
        (save-excursion
          (if (not (looking-at " "))
                   (re-search-backward "[^0-9]"))
          (list month
                (string-to-int (buffer-substring (1+ (point)) (+ 4 (point))))
                year))
      (if (looking-at "\\*")
          (save-excursion
            (re-search-backward "[^*]")
            (if (looking-at ".\\*\\*")
                (list month calendar-starred-day year)
              (if error (error "Not on a date!"))))
        (if error (error "Not on a date!"))))))

;;重定义标记节日的函数
(defun mark-visible-calendar-date (date &optional mark)
  "Mark DATE in the calendar window with MARK.
MARK is either a single-character string or a face.
MARK defaults to diary-entry-marker."
  (if (calendar-date-is-legal-p date)
      (save-excursion
        (set-buffer calendar-buffer)
        (calendar-cursor-to-visible-date date)
        (let ((mark (or mark diary-entry-marker)))
          (if (stringp mark)
              (let ((buffer-read-only nil))
                (forward-char 6)
                (delete-char 1)
                (insert mark)
                (forward-char -5))
            (overlay-put
             (make-overlay  (point) (+ 4 (point))) 'face mark))))))

;;;重定义得到当前日历节日的函数，本来是一屏显示3个月，
;;;所以必须改变，否则一次将3个月的日期重复标记

(defun list-calendar-holidays ()
  "Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list calendar-notable-days.  Returns t if any
holidays are found, nil if not."
  (interactive)
  (message "Looking up holidays...")
  (let ((holiday-list (calendar-holiday-list))
        (m1 displayed-month)
        (y1 displayed-year)
        (m2 displayed-month)
        (y2 displayed-year))
    (if (not holiday-list)
        (progn
          (message "Looking up holidays...none found")
          nil)
      (set-buffer (get-buffer-create holiday-buffer))
      (setq buffer-read-only nil)
      (calendar-set-mode-line
       (if (= y1 y2)
           (format "Notable Dates from %s to %s, %d%%-"
                   (calendar-month-name m1) (calendar-month-name m2) y2)
         (format "Notable Dates from %s, %d to %s, %d%%-"
                 (calendar-month-name m1) y1 (calendar-month-name m2) y2)))
      (erase-buffer)
      (insert
       (mapconcat
        '(lambda (x) (concat (calendar-date-string (car x))
                             ": " (car (cdr x))))
        holiday-list "\n"))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer holiday-buffer)
      (message "Looking up holidays...done")
      t)))

;;只显示当前这个月的节日
 (defun list-holidays (y1 y2 &optional l label)
  "Display holidays for years Y1 to Y2 (inclusive).

The optional list of holidays L defaults to `calendar-holidays'.  See the
documentation for that variable for a description of holiday lists.

The optional LABEL is used to label the buffer created."
  (interactive
   (let* ((start-year (calendar-read
                       "Starting year of holidays (>0): "
                       '(lambda (x) (> x 0))
                       (int-to-string (extract-calendar-year
                                       (calendar-current-date)))))
          (end-year (calendar-read
                       (format "Ending year (inclusive) of holidays (>=%s): "
                               start-year)
                       '(lambda (x) (>= x start-year))
                       (int-to-string start-year)))
          (completion-ignore-case t)
          (lists
           (list
            (cons "All" calendar-holidays)
            (if (fboundp 'atan)
                (cons "Equinoxes/Solstices"
                      (list (list 'solar-equinoxes-solstices))))
            (if general-holidays (cons "General" general-holidays))
            (if local-holidays (cons "Local" local-holidays))
            (if other-holidays (cons "Other" other-holidays))
            (if christian-holidays (cons "Christian" christian-holidays))
            (if hebrew-holidays (cons "Hebrew" hebrew-holidays))
            (if islamic-holidays (cons "Islamic" islamic-holidays))
            (if oriental-holidays (cons "Oriental" oriental-holidays))
            (if solar-holidays (cons "Solar" solar-holidays))
            (cons "Ask" nil)))
          (choice (capitalize
                   (completing-read "List (TAB for choices): " lists nil t)))
          (which (if (string-equal choice "Ask")
                     (eval (read-variable "Enter list name: "))
                   (cdr (assoc choice lists))))
          (name (if (string-equal choice "Equinoxes/Solstices")
                    choice
                  (if (member choice '("Ask" ""))
                      "Holidays" 
                    (format "%s Holidays" choice)))))
     (list start-year end-year which name)))
  (message "Computing holidays...")
  (let* ((holiday-buffer "*Holidays*")
         (calendar-holidays (if l l calendar-holidays))
         (title (or label "Holidays"))
         (holiday-list nil)
         (s (calendar-absolute-from-gregorian (list 1 1 y1)));;从第一个月开始
         (e (calendar-absolute-from-gregorian (list 12 1 y2)));;到最后一个月结束
         (d s);;算法中的月份
         (never t)
         (displayed-month 1)
         (displayed-year y1))
    (while (or never (<= d e))
      (setq holiday-list (append holiday-list (calendar-holiday-list)))
      (setq never nil)
      (increment-calendar-month displayed-month displayed-year 1)
      ;;一次只搜索一个月
      (setq d (calendar-absolute-from-gregorian
               (list displayed-month 1 displayed-year))))
    (save-excursion
      (set-buffer (get-buffer-create holiday-buffer))
      (setq buffer-read-only nil)
      (calendar-set-mode-line
       (if (= y1 y2)
           (format "%s for %s" title y1)
         (format "%s for %s-%s" title y1 y2)))
      (erase-buffer)
      (goto-char (point-min))
      (insert
       (mapconcat
        '(lambda (x) (concat (calendar-date-string (car x))
                             ": " (car (cdr x))))
        holiday-list "\n"))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer holiday-buffer)
      (message "Computing holidays...done"))))

;;;重定义每个节日是否在当前显示的函数
(defun holiday-fixed (month day string)
  "Holiday on MONTH, DAY (Gregorian) called STRING.
If MONTH, DAY is visible, the value returned is the list (((MONTH DAY year)
STRING)).  Returns nil if it is not visible in the current calendar window."
  (let ((m displayed-month)
        (y displayed-year))
  
    (if (= m month)
      (list (list (list month day y) string)))))

(defun holiday-float (month dayname n string &optional day)
  "Holiday on MONTH, DAYNAME (Nth occurrence) called STRING.
If the Nth DAYNAME in MONTH is visible, the value returned is the list
\(((MONTH DAY year) STRING)).

If N<0, count backward from the end of MONTH.

An optional parameter DAY means the Nth DAYNAME on or after/before MONTH DAY.

Returns nil if it is not visible in the current calendar window."
  (let ((m displayed-month)
        (y displayed-year))
  (if (= m month)
      (list (list (calendar-nth-named-day n dayname month y day) string)))))


(defvar diary-entries-list)
(defvar original-date)
(defvar date-string)
;;重定义日记标记的函数

(defun mark-sexp-diary-entries ()
  "Mark days in the calendar window that have sexp diary entries.
Each entry in the diary file (or included files) visible in the calendar window
is marked.  See the documentation for the function `list-sexp-diary-entries'."
  (let* ((sexp-mark (regexp-quote sexp-diary-entry-symbol))
         (s-entry (concat "\\(\\`\\|\^M\\|\n\\)\\("
                          (regexp-quote sexp-mark) "(\\)\\|\\("
                          (regexp-quote diary-nonmarking-symbol)
                          (regexp-quote sexp-mark) "(diary-remind\\)"))
         (m)
         (y)
         (first-date)
         (last-date))
    (save-excursion
      (set-buffer calendar-buffer)
      (setq m displayed-month)
      (setq y displayed-year))
    (setq first-date
          (calendar-absolute-from-gregorian (list m 1 y)))
    (setq last-date
          (calendar-absolute-from-gregorian
           (list m (calendar-last-day-of-month m y) y)))
    (goto-char (point-min))
    (while (re-search-forward s-entry nil t)
      (if (char-equal (preceding-char) ?\()
          (setq marking-diary-entry t)
        (setq marking-diary-entry nil))
      (re-search-backward "(")
      (let ((sexp-start (point))
            (sexp)
            (entry)
            (entry-start)
            (line-start))
        (forward-sexp)
        (setq sexp (buffer-substring-no-properties sexp-start (point)))
        (save-excursion
          (re-search-backward "\^M\\|\n\\|\\`")
          (setq line-start (point)))
        (forward-char 1)
        (if (and (or (char-equal (preceding-char) ?\^M)
                     (char-equal (preceding-char) ?\n))
                 (not (looking-at " \\|\^I")))
            (progn;; Diary entry consists only of the sexp
              (backward-char 1)
              (setq entry ""))
          (setq entry-start (point))
          ;; Find end of entry
          (re-search-forward "\^M\\|\n" nil t)
          (while (looking-at " \\|\^I")
            (or (re-search-forward "\^M\\|\n" nil t)
                (re-search-forward "$" nil t)))
          (if (or (char-equal (preceding-char) ?\^M)
                  (char-equal (preceding-char) ?\n))
              (backward-char 1))
          (setq entry (buffer-substring-no-properties entry-start (point)))
          (while (string-match "[\^M]" entry)
            (aset entry (match-beginning 0) ?\n )))
        (calendar-for-loop date from first-date to last-date do
          (if (diary-sexp-entry sexp entry
                                (calendar-gregorian-from-absolute date))
              (mark-visible-calendar-date
              (calendar-gregorian-from-absolute date))))))))
;;;重新定义fancy-diary 的日期标记
(defun fancy-diary-display ()
  "Prepare a diary buffer with relevant entries in a fancy, noneditable form.
This function is provided for optional use as the `diary-display-hook'."
  (save-excursion;; Turn off selective-display in the diary file's buffer.
    (set-buffer (find-buffer-visiting (substitute-in-file-name diary-file)))
    (let ((diary-modified (buffer-modified-p)))
      (subst-char-in-region (point-min) (point-max) ?\^M ?\n t)
      (setq selective-display nil)
      (kill-local-variable 'mode-line-format)
      (set-buffer-modified-p diary-modified)))
  (if (or (not diary-entries-list)
          (and (not (cdr diary-entries-list))
               (string-equal (car (cdr (car diary-entries-list))) "")))
      (let* ((holiday-list (if holidays-in-diary-buffer
                               (check-calendar-holidays original-date)))
             (msg (format "No diary entries for %s %s"
                          (concat date-string (if holiday-list ":" ""))
                          (mapconcat 'identity holiday-list "; "))))
        (if (<= (length msg) (frame-width))
            (message "%s" msg)
          (set-buffer (get-buffer-create holiday-buffer))
          (setq buffer-read-only nil)
          (calendar-set-mode-line date-string)
          (erase-buffer)
          (insert (mapconcat 'identity holiday-list "\n"))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (display-buffer holiday-buffer)
          (message  "No diary entries for %s" date-string)))
    (save-excursion;; Prepare the fancy diary buffer.
      (set-buffer (make-fancy-diary-buffer))
      (setq buffer-read-only nil)
      (let ((entry-list diary-entries-list)
            (holiday-list)
            (holiday-list-last-month 1)
            (holiday-list-last-year 1)
            (date (list 0 0 0)))
        (while entry-list
          (if (not (calendar-date-equal date (car (car entry-list))))
              (progn
                (setq date (car (car entry-list)))
                (and holidays-in-diary-buffer
                     (calendar-date-compare
                      (list (list holiday-list-last-month
                                  (calendar-last-day-of-month
                                   holiday-list-last-month
                                   holiday-list-last-year)
                                  holiday-list-last-year))
                      (list date))
              ;; We (do not) need to get the holidays for the next 3 months.
                     (setq holiday-list-last-month
                           (extract-calendar-month date))
                     (setq holiday-list-last-year
                           (extract-calendar-year date))
                     (setq holiday-list
                           (let ((displayed-month holiday-list-last-month)
                                 (displayed-year holiday-list-last-year))
                             (calendar-holiday-list)))
                     )
                (let* ((date-string (calendar-date-string date))
                       (date-holiday-list
                        (let ((h holiday-list)
                              (d))
                          ;; Make a list of all holidays for date.
                          (while h
                            (if (calendar-date-equal date (car (car h)))
                                (setq d (append d (cdr (car h)))))
                            (setq h (cdr h)))
                          d)))
                  (insert (if (= (point) (point-min)) "" ?\n) date-string)
                  (if date-holiday-list (insert ":  "))
                  (let* ((l (current-column))
                         (longest 0))
                    (insert (mapconcat (lambda (x)
                                         (if (< longest (length x))
                                             (setq longest (length x)))
                                         x)
                                       date-holiday-list
                                       (concat "\n" (make-string l ? ))))
                    (insert ?\n (make-string (+ l longest) ?=) ?\n)))))
          (if (< 0 (length (car (cdr (car entry-list)))))
              (insert (car (cdr (car entry-list))) ?\n))
          (setq entry-list (cdr entry-list))))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (display-buffer fancy-diary-buffer)
      (message "Preparing diary...done"))))

  (defun mark-calendar-days-named (dayname)
  "Mark all dates in the calendar window that are day DAYNAME of the week.
0 means all Sundays, 1 means all Mondays, and so on."
  (save-excursion
    (set-buffer calendar-buffer)
    (let ((prev-month displayed-month)
          (prev-year displayed-year)
          (succ-month displayed-month)
          (succ-year displayed-year)
          (last-day)
          (day))
      (setq day (calendar-absolute-from-gregorian
                 (calendar-nth-named-day 1 dayname prev-month prev-year)))
      (setq last-day (calendar-absolute-from-gregorian
                 (calendar-nth-named-day -1 dayname succ-month succ-year)))
      (while (<= day last-day)
        (mark-visible-calendar-date (calendar-gregorian-from-absolute day))
        (setq day (+ day 7))))))

(defun mark-calendar-date-pattern (month day year &optional color)
  "Mark all dates in the calendar window that conform to MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard."
  (save-excursion
    (set-buffer calendar-buffer)
    (let ((m displayed-month)
          (y displayed-year))
          (mark-calendar-month m y month day year color);;21.3用该函数
          ;(mark-calendar-month m y month day year);;21.2用该函数
          )))

(defun solar-equinoxes-solstices ()
  "*local* date and time of equinoxes and solstices, if visible in the calendar window.
Requires floating point."
  (let ((m displayed-month)
        (y displayed-year))
    (let* ((calendar-standard-time-zone-name
            (if calendar-time-zone calendar-standard-time-zone-name "UTC"))
           (calendar-daylight-savings-starts
            (if calendar-time-zone calendar-daylight-savings-starts))
           (calendar-daylight-savings-ends
            (if calendar-time-zone calendar-daylight-savings-ends))
           (calendar-time-zone (if calendar-time-zone calendar-time-zone 0))
           (k (1- (/ m 3)))
           (d0 (solar-equinoxes/solstices k y)) 
           (d1 (list (car d0) (floor (car (cdr d0))) (car (cdr (cdr d0)))))
           (h0 (* 24 (- (car (cdr d0)) (floor (car (cdr d0))))))
           (adj (dst-adjust-time d1 h0))
           (d (list (car d1) (+ (car (cdr d1))  
                  (/ (car (cdr adj)) 24.0))
                    (car (cdr (cdr d1)))))
           (abs-day (calendar-absolute-from-gregorian d)))
      (list
       (list (calendar-gregorian-from-absolute (floor abs-day))
             (format "%s %s"
                     (nth k (if (and calendar-latitude
                                     (< (calendar-latitude) 0))
                                solar-s-hemi-seasons
                              solar-n-hemi-seasons))
                     (solar-time-string
                      (* 24 (- abs-day (floor abs-day)))
                      (if (dst-in-effect abs-day)
                          calendar-daylight-time-zone-name
                       calendar-standard-time-zone-name))))))))
;;;
;;;定义阴历节日,添加至other-holiday列表中

(defun chinese-lunar-day(cmonth cday cname)
"Chinese calendar holiday, month and day in Chinese calendar (CMONTH, CDAY).

If corresponding MONTH and DAY in gregorian calendar is visible,
the value returned is the list \(((MONTH DAY year) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let* ((m displayed-month)
         (y displayed-year)
         (gdate (calendar-gregorian-from-absolute
                 (+ (cadr (assoc cmonth (chinese-year y))) (1- cday))))
         (gm (car gdate))
         (gd (cadr gdate))
         (gy (caddr gdate)))

    (if (= m gm)
        (list (list (list gm gd gy) cname)))))

;;;定义中国的节日
(setq general-holidays 
      '((holiday-fixed 1 1 "元旦")
        (holiday-chinese-new-year )
        (holiday-fixed 3 8 "妇女节")
        (holiday-fixed 3 12 "植树节")
        (holiday-fixed 5 1 "劳动节")
        (holiday-fixed 5 4 "青年节")
        (holiday-fixed 6 1 "儿童节")
        (holiday-fixed 9 10 "教师节")
        (holiday-fixed 10 1 "国庆节")))

(setq local-holidays
      '((chinese-lunar-day 1 15 "元宵节")
        (chinese-lunar-day 3 27 "我的生日")
        (chinese-lunar-day 5 5 "端午节")
        (chinese-lunar-day 9 9 "重阳节")
        (chinese-lunar-day 8 15 "中秋节")
        )
)
(setq christian-holidays nil)
(setq hebrew-holidays  nil)
(setq islamic-holidays nil)

(setq other-holidays 
      '((holiday-fixed 2 14 "情人节")
        (holiday-fixed 4 1 "愚人节")
        (holiday-fixed 12 25 "圣诞节")
        (holiday-float 5 0 2 "母亲节")
        (holiday-float 6 0 3 "父亲节")
        (holiday-float 11 4 4 "感恩节")))

(setq calendar-holidays
      (append general-holidays local-holidays other-holidays))

(defun holiday-chinese-new-year ()
  "Date of Chinese New Year."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y 1)
    (if (< m 5)
        (let ((chinese-new-year
               (calendar-gregorian-from-absolute
                (car (cdr (assoc 1 (chinese-year y)))))))
          (if (calendar-date-is-visible-p chinese-new-year)
          (list
           (list chinese-new-year
                 (format "%s年春节"
                         (calendar-chinese-sexagesimal-name (+ y 57))))))))))

(setq solar-n-hemi-seasons
  '( "春分" "夏至" "秋分"  "冬至"))
(setq solar-s-hemi-seasons
  '("秋分" "夏至" "春分" "冬至"))

;;重新绑定一些键值
  (define-key calendar-mode-map [prior] 'scroll-calendar-right)
;;一次只移动一个月
  (define-key calendar-mode-map "\ev"   'scroll-calendar-right)
  (define-key calendar-mode-map [next]  'scroll-calendar-left)
  (define-key calendar-mode-map "\C-v"  'scroll-calendar-left)
;;按中国习惯，周一为每周的第一天
 (setq calendar-week-start-day 1)   
(provide 'chinese-calendar)
;;; chinese-calendar.el ends here