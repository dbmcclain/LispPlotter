LispPlotter
===========

Publication-quality 2-D Data Plotting in Lisp

![Example](https://github.com/dbmcclain/LispPlotter/assets/3160577/2e312dbc-c592-4e7f-9845-1eab6459cc4c)

[Example.pdf](https://github.com/dbmcclain/LispPlotter/files/14318921/Example.pdf)

MacOS produces gorgeous Display-PDF images on screen. Using the pane popup-menu, they can be directly copied and pasted into other programs, or saved to PDF files.

MacOSX, starting in Catalina and above, introduced severe restrictions on when you can draw to screen. It must happen *ONLY* during a redraw-callback, running on the main drawing thread, and nowhere else - else it bombs out badly. The graphics engine is still, since 1984, only single-threaded by design, and all drawing must occur on the main thread of an application. Despite this pretty severe restriction, we can do quite a lot of useful work.

The new code tries to avoid having unnecessary non-drawing activity take place in that main thread. It performs all pre-scaling, filtering for NaN's and Infinities, interpolating Spline curves for smooth display, etc., all on any other thread but the Main Mac drawing thread. We want to keep that lightly loaded so that the overall system remains responsive.

Using Actors, it is trivial to invoke multiple concurrent parallel threads to perform much of this work. Just SEND to a FORK of all the actions needed. The JOIN is automatic, before sending final results to the customer Actor.

This Plotter code is not overtly Actor driven, but it is Actor-aware, and can usefully signal customer Actors waiting on the redraw to screen. You can schedule a whole slew of plotting activity, and use WITH-DELAYED-UPDATE :NOTIFYING when it has completed. This avoids choppy partial drawings appearing, and presents the final image all in one go. 

The update rate of Plotter is sufficient for live video productions of changing data. I get up to 30 Hz refresh rates in my live telemetry system, just performing PLOT of the data, which completely clears out the previous plot and draws the whole thing all over again with fresh data.

The code defines a PLOTTER-PANE which can act alone in a Window, or be used within your own Interface designs, including those with several simultaneous PLOTTER-PANE displays. It is a CAPI:PANE object like any other. PLOTTER-PANE is an augmentation of a CAPI:OUTPUT-PANE.
