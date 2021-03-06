module Helper.Sidebar where

import Import
import Helper.MakeBrief

sidebarWidget :: Widget
sidebarWidget = do
  articleArchives <- handlerToWidget $ runDB $ selectList [] [Desc ArticleCreatedAt, LimitTo 10]
  $(widgetFile "inline/sidebar")