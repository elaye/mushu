# Structure

This is app relies on [Brick](https://hackage.haskell.org/package/brick) for the user interface and on [libmpd](https://hackage.haskell.org/package/libmpd) for the communication with an MPD server.

## UI

The UI is made using the awesome [Brick](https://hackage.haskell.org/package/brick) library.

The UI is defined in terms of "views" and "widgets".
A view acts as a container for what is displayed on the screen.
There can be only one active view at all times.

### Views

A view is basically a "container" widget responsible for organizing different widgets.
A view delegates events to the widgets it contains depending on the current app state.
For example, the library view normally passes keyboard events to the library widget except if the filter is focused.
In that case the keyboard events are passed to the filter widget in order to edit the filter.
There are only two views at the moment: the library view and the playlist view.
Each view defines the `event` and `draw` functions.
The `event` function is used to handle the events passed by Brick.
The `draw` function is used to draw the view.
A view can modify the app state as opposed to the widgets described below.

### Widgets

A widget is a wrapper around a Brick widget.
A widget has its own state and it can handle events. A widget generally has the following functions:
`mkState`, `mkWidget`, `handleEvent` and `attrs`.

 - `mkState` is used to create the data type representing the widget state
 - `mkWidget` is used to create the Brick widget based on the widget state
 - `handleEvent` handles the widget events passed from the parent view
 - `attrs` is used by the global attributes map for styling

## Events

There are two types of events handled by the app: Brick events (keyboard events) and MPD events.
The MPD events are handled using custom Brick events.
The app is waiting for MPD events in a separate thread (using the `idle` function). When an event is received, it is written to a `BChan` that will pass it to the main event loop and propagate it down the views and widgets.
The flow of events is unidirectional, meaning that if a keyboard event triggers an MPD action (pausing the music for example), the function that sends the action to MPD doesn't wait for an answer and doesn't update the app state.
When an action is completed, MPD sends an update which will be caught by the MPD event loop then triggering the update of the app state.

