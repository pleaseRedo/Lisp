# Calculator
A text-based interface calculator!!

Feature:
Perform operation + - * /
e.g input: (+3) (+2)
    output (+5)
    they can be nasted ofc.
    
Operations on variables:
e.g input: '(+ 1XY + Y) (- 3X + 2y)
    output:  (+ 1XY + Y + |-3X|)
    captial is not sensative.
    
Code is hard to read:

In order to make the output more readable, I made lot of effort on re-ordering the output from (+ (1 2)) to (1 + 2) etc.
"So the code looks extremely hard to understand". This is what I got from the department and I will take that as a commendation.
 
More examples 
Input: '(+ a + b + c + d + e)  '(+ f + g + h + i + j + k) 
Output: (+ A + B + C + D + E + F + G + H + I + J + K)

Input:  '(+ 1XY + Y + 4) '(- 3X + Z + Y - 3)
Output: (+ 1XY + |2Y| + |-3X| + Z + 1)


    
