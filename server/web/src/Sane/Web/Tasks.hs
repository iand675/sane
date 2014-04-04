module Sane.Web.Tasks where
import           Network.Webmachine
import           Sane.Common
import           Sane.Models.Common
import           Sane.Models.Tasks
import           Sane.Web.Miscellaneous (api)
import           Prelude (undefined)

listTasks :: Resource AppConfig s a () (Result Task)
listTasks = api undefined

createTask :: Resource AppConfig s a () (Result Task)
createTask = api undefined

getTask :: Resource AppConfig s a () (Result Task)
getTask = api undefined

updateTask :: Resource AppConfig s a () (Result Task)
updateTask = api undefined

deleteTask :: Resource AppConfig s a () ()
deleteTask = basic undefined
