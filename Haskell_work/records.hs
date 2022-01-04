-- Exercise 7.1
{- 
  data ABCD
      = A { foo :: String, bar :: Int }
      | B { foo :: String, baz :: () }
      | C Int
      | D
-}
-- foo has type ABCD -> String
-- bar has type ABCD -> Int
-- baz has type ABCD -> ()

-- Exercise 7.2
data Data = One { data :: Int } | Two { data :: Bool }
-- Does not work because data cannot be a function that goes form Data -> Int and Data -> Bool

-- Exercise 7.3

data Person
  = Student
      { firstName :: String
      , lastName :: String
      , id :: String
      , major :: String
      , year :: Int
      , courses_enrolled :: [(String, (Int, Int))]
      }
  | Teacher
      { firstName :: String
      , lastName :: String
      , dept :: String
      , courses_teaching :: [(Int, Int)]
      }

------------------------------------------------------------------------------

{- names :: Person -> (String, String)
names (Student {firstName = x, lastName = y}) = (y,x)

classes :: [Person] -> (Int, Int) -> [(String, String)]
classes [] teacher = []
classes (st:sts) teacherCourses = 
  if snd(head (courses_enrolled (st))) == teacherCourses 
  then names st : (classes sts teacherCourses)
  else classes sts teacherCourses

studentsOfTeacher_ :: [Person] -> Person -> [[(String, String)]]
studentsOfTeacher_ students teacher = map (classes students) (courses_teaching teacher)

studentsOfTeacher :: Person -> [[(String, String)]]
studentsOfTeacher = studentsOfTeacher_ allStudents
-}
studentsInCourse :: (String, (Int, Int)) -> [Person] -> [(String, String)]
studentsInCourse course students = 
    [(firstName student, lastName student)
    | student <- students
    , elem course (courses_enrolled student)
    ]

studentsOfTeacher_ :: [Person] -> Person -> [((Int, Int), [(String, String)])]
studentsOfTeacher_ students teacher = 
  let dep = dept teacher in 
    [ ((course), (studentsInCourse (dep, course) students))
    | course <- courses_teaching teacher
    ]

studentsOfTeacher:: Person -> [((Int, Int), [(String, String)])]
studentsOfTeacher = studentsOfTeacher_ allStudents
------------------------------------------------------------------------------

professorChugh =
  Teacher "Ravi" "Chugh" "CMSC" [(16100,1)]

professorKurtz =
  Teacher "Stuart" "Kurtz" "CMSC" [(16100,2), (28000,1)]

allStudents =
  [ Student "A" "Student" "********" "CMSC" 1 [("CMSC", (15100,1))]
  , Student "B" "Student" "********" "CMSC" 1 [("CMSC", (16100,1))]
  , Student "C" "Student" "********" "CMSC" 2 [("CMSC", (16100,2))]
  , Student "D" "Student" "********" "MATH" 2 [("CMSC", (28000,1))]
  , Student "E" "Student" "********" "MATH" 3 [("CMSC", (28000,1))]
  , Student "F" "Student" "********" "ARTV" 3 [("CMSC", (12100,1))]
  , Student "STEAM" "Student" "********" "ARTV" 4
      [("CMSC", (16100,1)), ("ARTV", (22500,1)), ("ARTV", (22502,1))]
  ]