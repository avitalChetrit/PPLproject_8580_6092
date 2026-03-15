<div dir="rtl" align="right">

# תיעוד טכני ומדריך פקודות – פרויקט VM Translator

**מחברים:** טל שזיפי, אביטל חזן

מסמך זה מפרט את תפקידי הפקודות והלוגיקה ששימשו לבניית המתרגם בשפת **Clojure**.
המסמך נועד לספק הסבר מעמיק על אופן פעולת הקוד והתחביר הייחודי שלו.

---

# 1. ניתוח פונקציות העזר (Helper Functions)

## פקודות לוגיות (כמו <span dir="ltr">`handleEq`</span> ו־<span dir="ltr">`handleGt`</span>)

<table width="100%">
<tr>
<td valign="top">
<ul style="direction: ltr; text-align: left; list-style-position: inside;">
  <li><span dir="ltr">**`defn`**</span> – הגדרת פונקציה חדשה.</li>
  <li><span dir="ltr">**`[writer counter-atom]`**</span> – הפרמטרים שהפונקציה מקבלת.</li>
  <li><span dir="ltr">**`swap!`**</span> – עדכון ערך של אטום (**Atom**).</li>
  <li><span dir="ltr">**`inc`**</span> – מעלה את הערך המספרי ב־1.</li>
  <li><span dir="ltr">**`@`**</span> – שליפת הערך הנוכחי מתוך האטום.</li>
</ul>
</td>
<td valign="top" style="text-align: right; direction: rtl;">
<ul style="list-style-position: inside;">
  <li>הגדרת פונקציה חדשה בעברית.</li>
  <li>פרמטרים שהפונקציה מקבלת.</li>
  <li>עדכון ערך של אטום (**Atom**).</li>
  <li>מעלה את הערך המספרי ב־1.</li>
  <li>שליפת הערך הנוכחי מתוך האטום.</li>
</ul>
</td>
</tr>
</table>

---

## פקודות זיכרון (כמו <span dir="ltr">`handlePush`</span> ו־<span dir="ltr">`handlePop`</span>)

<table width="100%">
<tr>
<td valign="top">
<ul style="direction: ltr; text-align: left; list-style-position: inside;">
  <li><span dir="ltr">**`str`**</span> – חיבור מחרוזות.</li>
  <li><span dir="ltr">**`write`**</span> – כתיבה לקובץ באמצעות Java.</li>
</ul>
</td>
<td valign="top" style="text-align: right; direction: rtl;">
<ul style="list-style-position: inside;">
  <li>חיבור מחרוזות.</li>
  <li>כתיבה לקובץ.</li>
</ul>
</td>
</tr>
</table>

---

# 2. עיבוד טקסט ופירוק שורות (Parsing)

<table width="100%">
<tr>
<td valign="top">
<ul style="direction: ltr; text-align: left; list-style-position: inside;">
  <li><span dir="ltr">**`str/trim`**</span> – ניקוי רווחים, טאבים וירידות שורה.</li>
  <li><span dir="ltr">**`str/split`**</span> – חיתוך מחרוזת לרשימת מילים.</li>
  <li><span dir="ltr">**`#"\s+"`**</span> – ביטוי רגולרי לחיתוך רווחים.</li>
  <li><span dir="ltr">**`let`**</span> – יצירת משתנה זמני עם רשימת מילים.</li>
</ul>
</td>
<td valign="top" style="text-align: right; direction: rtl;">
<ul style="list-style-position: inside;">
  <li>ניקוי רווחים, טאבים וירידות שורה.</li>
  <li>חיתוך מחרוזת לרשימת מילים.</li>
  <li>ביטוי רגולרי לחיתוך רווחים.</li>
  <li>יצירת משתנה זמני עם רשימת מילים.</li>
</ul>
</td>
</tr>
</table>

---

# 3. לוגיקה ותנאים

<table width="100%">
<tr>
<td valign="top">
<ul style="direction: ltr; text-align: left; list-style-position: inside;">
  <li><span dir="ltr">**`when`**</span> – ריצה רק אם התנאי אמת.</li>
  <li><span dir="ltr">**`and`**</span> – ריצה אם כל התנאים נכונים.</li>
  <li><span dir="ltr">**`seq`**</span> – בדיקה אם הרצף אינו ריק.</li>
  <li><span dir="ltr">**`nil`**</span> – ערך ריק או חוסר ערך.</li>
</ul>
</td>
<td valign="top" style="text-align: right; direction: rtl;">
<ul style="list-style-position: inside;">
  <li>ריצה רק אם התנאי אמת.</li>
  <li>ריצה אם כל התנאים נכונים.</li>
  <li>בדיקה אם הרצף אינו ריק.</li>
  <li>ערך ריק או חוסר ערך.</li>
</ul>
</td>
</tr>
</table>

---

# 4. עבודה עם קבצים (I/O)

<table width="100%">
<tr>
<td valign="top">
<ul style="direction: ltr; text-align: left; list-style-position: inside;">
  <li><span dir="ltr">**`io/file`**</span> – יצירת אובייקט קובץ.</li>
  <li><span dir="ltr">**`with-open`**</span> – פתיחת משאב וסגירה אוטומטית.</li>
  <li><span dir="ltr">**`line-seq`**</span> – יצירת רצף שורות לעיבוד.</li>
  <li><span dir="ltr">**`doseq`**</span> – לולאה עם תופעות לוואי.</li>
  <li><span dir="ltr">**`.exists` / `.isDirectory`**</span> – בדיקת קיום ותיקייה.</li>
  <li><span dir="ltr">**`.listFiles`**</span> – רשימת הקבצים בתיקייה.</li>
</ul>
</td>
<td valign="top" style="text-align: right; direction: rtl;">
<ul style="list-style-position: inside;">
  <li>יצירת אובייקט קובץ.</li>
  <li>פתיחת משאב וסגירה אוטומטית.</li>
  <li>יצירת רצף שורות לעיבוד.</li>
  <li>לולאה עם תופעות לוואי.</li>
  <li>בדיקת קיום ותיקייה.</li>
  <li>רשימת הקבצים בתיקייה.</li>
</ul>
</td>
</tr>
</table>

---

# 5. אינטראקציה עם המשתמש

<table width="100%">
<tr>
<td valign="top">
<ul style="direction: ltr; text-align: left; list-style-position: inside;">
  <li><span dir="ltr">**`& args`**</span> – קבלת פרמטרים משתנים לשורת הפקודה.</li>
  <li><span dir="ltr">**`flush`**</span> – ניקוי Buffer והדפסת הטקסט מיידית.</li>
  <li><span dir="ltr">**`read-line`**</span> – עצירת ריצה וקבלת קלט מהמשתמש.</li>
</ul>
</td>
<td valign="top" style="text-align: right; direction: rtl;">
<ul style="list-style-position: inside;">
  <li>קבלת פרמטרים משתנים לשורת הפקודה.</li>
  <li>ניקוי Buffer והדפסת הטקסט מיידית.</li>
  <li>עצירת ריצה וקבלת קלט מהמשתמש.</li>
</ul>
</td>
</tr>
</table>

---

## סיכום

המסמך מציג את המבנה הלוגי של פרויקט **VM Translator** ואת הפקודות המרכזיות בשפת **Clojure** ששימשו ליישומו.
באמצעות מנגנוני **Parsing**, עבודה עם קבצים, ופונקציות עזר לתרגום פקודות, המערכת מסוגלת לקרוא קבצי <span dir="ltr">`.vm`</span> ולתרגם אותם לקוד **Assembly**.

---

**נכתב על ידי:**
טל שזיפי ואביטל חזן

</div>
