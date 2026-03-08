mash = ["cmd", "alt", "shift"]
mash2 = ["cmd", "alt", "ctrl"]

spacing = {
  x: 5
  y: 5
}

GRID_WIDTH = 2

changeGridWidth = (by_) ->
  GRID_WIDTH = Math.max(2, GRID_WIDTH + by_)
  api.alert "grid is now " + GRID_WIDTH + " tiles wide", 1
  _.each Window.visibleWindows(), (win) ->
    win.snapToGrid()

Window::getGrid = ->
  winFrame = @frame()
  screenRect = @screen().frameWithoutDockOrMenu()
  thirdScreenWidth = screenRect.width / GRID_WIDTH
  halfScreenHeight = screenRect.height / 2
  rect = {
    x: Math.round((winFrame.x - screenRect.x) / thirdScreenWidth)
    y: Math.round((winFrame.y - screenRect.y) / halfScreenHeight)
    w: Math.max(1, Math.round(winFrame.width / thirdScreenWidth))
    h: Math.max(1, Math.round(winFrame.height / halfScreenHeight))
  }
  rect

Window::setGrid = (grid, screen) ->
  screenRect = screen.frameWithoutDockOrMenu()
  thirdScreenWidth = screenRect.width / GRID_WIDTH
  halfScreenHeight = screenRect.height / 2
  rect = {
    x: (grid.x * thirdScreenWidth) + screenRect.x
    y: (grid.y * halfScreenHeight) + screenRect.y
    width: grid.w * thirdScreenWidth
    height: grid.h * halfScreenHeight
  }

  if grid.x > 0
    rect.x += spacing.x
    rect.width -= spacing.x

  if grid.y > 0
    rect.y += spacing.y
    rect.height -= spacing.y

  @setFrame rect

Window::snapToGrid = ->
  @setGrid @getGrid(), @screen()  if @isNormalWindow()

api.bind 'D', mash, ->
  api.launch "Dictionary"

api.bind ';', mash, ->
  Window.focusedWindow().snapToGrid()

api.bind "'", mash, ->
  _.each Window.visibleWindows(), (win) ->
    win.snapToGrid()

api.bind '=', mash, ->
  changeGridWidth +1

api.bind '-', mash, ->
  changeGridWidth -1

api.bind 'H', mash2, ->
  Window.focusedWindow().focusWindowLeft()

api.bind 'L', mash2, ->
  Window.focusedWindow().focusWindowRight()

api.bind 'K', mash2, ->
  Window.focusedWindow().focusWindowUp()

api.bind 'J', mash2, ->
  Window.focusedWindow().focusWindowDown()

api.bind 'M', mash, ->
  win = Window.focusedWindow()
  f = {
    x: 0
    y: 0
    w: GRID_WIDTH
    h: 2
  }
  win.setGrid f, win.screen()

api.bind "N", mash, ->
  win = Window.focusedWindow()
  win.setGrid win.getGrid(), win.screen().nextScreen()

api.bind "P", mash, ->
  win = Window.focusedWindow()
  win.setGrid win.getGrid(), win.screen().previousScreen()

api.bind "H", mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.x = Math.max(f.x - 1, 0)
  win.setGrid f, win.screen()

api.bind 'L', mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.x = Math.min(f.x + 1, GRID_WIDTH - f.w)
  win.setGrid f, win.screen()

api.bind 'O', mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.w = Math.min(f.w + 1, GRID_WIDTH - f.x)
  win.setGrid f, win.screen()

api.bind 'I', mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.w = Math.max(f.w - 1, 1)
  win.setGrid f, win.screen()

api.bind 'J', mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.y = 1
  f.h = 1
  win.setGrid f, win.screen()

api.bind 'K', mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.y = 0
  f.h = 1
  win.setGrid f, win.screen()

api.bind 'U', mash, ->
  win = Window.focusedWindow()
  f = win.getGrid()
  f.y = 0
  f.h = 2
  win.setGrid f, win.screen()

# Local Variables:
# eval: (coffee-cos-mode 1)
# End:
