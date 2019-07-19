module Notif exposing
    ( Notif
    , NotifType(..)
    , getDescription
    , getNotifType
    , makeNotification
    )


type NotifType
    = Error
    | Warning
    | Info


type Notif
    = Notif String NotifType



-- INITIALIZATION


makeNotification : String -> NotifType -> Notif
makeNotification description notifType =
    Notif description notifType



-- EXPORT


getDescription : Notif -> String
getDescription (Notif description _) =
    description


getNotifType : Notif -> NotifType
getNotifType (Notif _ notifType) =
    notifType
