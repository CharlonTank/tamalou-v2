module Evergreen.V54.InternalAnim.Timeline exposing (..)

import Evergreen.V54.InternalAnim.Time


type Occurring event
    = Occurring event Evergreen.V54.InternalAnim.Time.Absolute Evergreen.V54.InternalAnim.Time.Absolute


type Line event
    = Line Evergreen.V54.InternalAnim.Time.Absolute (Occurring event) (List (Occurring event))


type Timetable event
    = Timetable (List (Line event))


type Event event
    = Event Evergreen.V54.InternalAnim.Time.Duration event (Maybe Evergreen.V54.InternalAnim.Time.Duration)


type Schedule event
    = Schedule Evergreen.V54.InternalAnim.Time.Duration (Event event) (List (Event event))


type alias TimelineDetails event =
    { initial : event
    , now : Evergreen.V54.InternalAnim.Time.Absolute
    , updatedAt : Evergreen.V54.InternalAnim.Time.Absolute
    , delay : Evergreen.V54.InternalAnim.Time.Duration
    , scale : Float
    , events : Timetable event
    , queued : Maybe (Schedule event)
    , interruption : List (Schedule event)
    , running : Bool
    }


type Timeline event
    = Timeline (TimelineDetails event)
