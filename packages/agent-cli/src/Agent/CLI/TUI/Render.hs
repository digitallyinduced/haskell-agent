-- | Rendering for the retained fullscreen terminal application.
--
-- The implementation lives behind an internal boundary so the stable public
-- rendering API remains small and independent from its Brick widget graph.
module Agent.CLI.TUI.Render
    ( module Internal
    ) where

import Agent.CLI.TUI.Render.Internal as Internal
