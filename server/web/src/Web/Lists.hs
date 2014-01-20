module Web.Lists where

import Data.Void
import qualified Domain.Types
import Router (SaneAction(..))
import Web.Types

type ListAction = WebmachineM

createList :: NewList -> ListAction List
createList = undefined

getList :: Id List -> ListAction List
getList = undefined

listLists :: ListAction [List]
listLists = undefined

updateList :: Id List -> ListPatch -> ListAction List
updateList = undefined

deleteList :: Id List -> ListAction Void
deleteList = undefined

getMembership :: Id List -> Username -> ListAction Membership
getMembership = undefined

setMembership :: Id List -> Username -> MembershipKind -> ListAction Membership
setMembership = undefined

deleteMembership :: Id List -> Username -> ListAction Membership
deleteMembership = undefined
