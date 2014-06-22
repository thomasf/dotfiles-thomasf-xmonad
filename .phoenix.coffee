changeGridWidth = (by_) ->
  GRID_WIDTH = Math.max(1, GRID_WIDTH + by_)
  api.alert "grid is now " + GRID_WIDTH + " tiles wide", 1
  _.each Window.visibleWindows(), (win) ->
    win.snapToGrid()
    return
  return

mash = [
  "cmd"
  "alt"
  "ctrl"
]

mashShift = [
  "cmd"
  "alt"
  "shift"
]

MARGIN_X = 5
MARGIN_Y = 5
GRID_WIDTH = 3

Window::getGrid = ->
  winFrame = @frame()
  screenRect = @screen().frameWithoutDockOrMenu()
  thirdScreenWidth = screenRect.width / GRID_WIDTH
  halfScreenHeight = screenRect.height / 2
  x: Math.round((winFrame.x - screenRect.x) / thirdScreenWidth)
  y: Math.round((winFrame.y - screenRect.y) / halfScreenHeight)
  w: Math.max(1, Math.round(winFrame.width / thirdScreenWidth))
  h: Math.max(1, Math.round(winFrame.height / halfScreenHeight))

Window::setGrid = (grid, screen) ->
  screenRect = screen.frameWithoutDockOrMenu()
  thirdScreenWidth = screenRect.width / GRID_WIDTH
  halfScreenHeight = screenRect.height / 2
  newFrame =
    x: (grid.x * thirdScreenWidth) + screenRect.x
    y: (grid.y * halfScreenHeight) + screenRect.y
    width: grid.w * thirdScreenWidth
    height: grid.h * halfScreenHeight

  newFrame.x += MARGIN_X
  newFrame.y += MARGIN_Y
  newFrame.width -= (MARGIN_X * 2.0)
  newFrame.height -= (MARGIN_Y * 2.0)
  @setFrame newFrame
  return

Window::snapToGrid = ->
  @setGrid @getGrid(), @screen()  if @isNormalWindow()
  return

api.bind "D", mash, ->
  api.launch "Dictionary"
  return

api.bind ";", mash, ->
  Window.focusedWindow().snapToGrid()
  return

api.bind "'", mash, ->
  _.each Window.visibleWindows(), (win) ->
    win.snapToGrid()
    return
  return

api.bind "=", mash, ->
  changeGridWidth +1
  return

api.bind "-", mash, ->
  changeGridWidth -1
  return

api.bind "H", mashShift, ->
  Window.focusedWindow().focusWindowLeft()
  return

api.bind "L", mashShift, ->
  Window.focusedWindow().focusWindowRight()
  return

api.bind "K", mashShift, ->
  Window.focusedWindow().focusWindowUp()
  return

api.bind "J", mashShift, ->
  Window.focusedWindow().focusWindowDown()
  return

api.bind "M", mash, ->
  win = Window.focusedWindow()
  f =
    x: 0
    y: 0
    w: GRID_WIDTH
    h: 2

  win.setGrid f, win.screen()
  true

api.bind "N", mash, ->
  win = Window.focusedWindow()
  win.setGrid win.getGrid(), win.screen().nextScreen()
  true

api.bind "P", mash, ->
  win = Window.focusedWindow()
  win.setGrid win.getGrid(), win.screen().previousScreen()
  true

api.bind "H", mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.x = Math.max(f.x - 1, 0)
  win.setGrid f, win.screen()
  true

api.bind "L", mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.x = Math.min(f.x + 1, GRID_WIDTH - f.w)
  win.setGrid f, win.screen()
  true

api.bind "O", mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.w = Math.min(f.w + 1, GRID_WIDTH - f.x)
  win.setGrid f, win.screen()
  true

api.bind "I", mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.w = Math.max(f.w - 1, 1)
  win.setGrid f, win.screen()
  true

api.bind "J", mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.y = 1
  f.h = 1
  win.setGrid f, win.screen()
  true

api.bind "K", mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.y = 0
  f.h = 1
  win.setGrid f, win.screen()
  true

api.bind "U", mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.y = 0
  f.h = 2
  win.setGrid f, win.screen()
  true


# Local Variables:
# eval: (coffee-cos-mode 1)
# End:
