port module Scroller exposing (initScroller, lazyLoad, scrollTo, scrollTop)

{-| Communicates with JS to scroll the page to a image. Also handles lazy
loading
-}

import Json.Encode as Encode exposing (Value)


{-| Tell JS to start listening to intersection events to lazy load the image
(used when switching between pages)
-}
port startLazyPort : () -> Cmd msg


lazyLoad : Cmd msg
lazyLoad =
    startLazyPort ()


{-| Tell JS to scroll to an image that contains the given name
-}
port scrollToPort : Value -> Cmd msg


scrollTo : String -> Cmd msg
scrollTo image =
    image
        |> Encode.string
        |> scrollToPort


{-| Tell JS to scroll to top of the page
-}
port scrollTopPort : () -> Cmd msg


scrollTop : Cmd msg
scrollTop =
    scrollTopPort ()


{-| Tell JS to init the big side scroller and show/hide it based on scrolling
-}
port initScrollerPort : () -> Cmd msg


initScroller : Cmd msg
initScroller =
    initScrollerPort ()
