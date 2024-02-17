LispPlotter
===========

Publication-quality 2-D Data Plotting in Lisp

![Example](https://github.com/dbmcclain/LispPlotter/assets/3160577/2e312dbc-c592-4e7f-9845-1eab6459cc4c)

[Example.pdf](https://github.com/dbmcclain/LispPlotter/files/14318921/Example.pdf)

MacOS produces gorgeous Display-PDF images on screen. Using the pane popup-menu, they can be directly copied and pasted into other programs, or saved to PDF files. The current code has been updated and improved to be compatible with the latest MacOSX Sonoma.

MacOSX, starting in Catalina and above, introduced severe restrictions on when you can draw to screen. It must happen *ONLY* during a redraw-callback, running on the main drawing thread, and nowhere else - else it bombs out badly. The graphics engine is still, since 1984, only single-threaded by design, and all drawing-related activity (obtaining and measuring fonts, making colors, measuring images, etc.) must occur on the main thread of an application. Despite this pretty severe restriction, we can do quite a lot of useful work.

The new code tries to avoid having unnecessary non-drawing activity take place in that main thread. It performs all data pre-scaling, filtering for NaN's and Infinities, interpolating Spline curves for smooth display, etc., all on any other thread but the Main Mac drawing thread. We want to keep that lightly loaded so that the overall system remains responsive.

Using Actors, it is trivial to invoke multiple concurrent parallel threads to perform much of this work. Just SEND to a FORK of all the actions needed. The JOIN is automatic, before sending final results to the customer Actor.

This Plotter code is not overtly Actor driven, but it is Actor-aware, and can usefully signal customer Actors waiting on the redraw to screen. You can schedule a whole slew of plotting activity, and use WITH-DELAYED-UPDATE :NOTIFYING when it has completed. This avoids choppy partial drawings appearing, and presents the final image all in one go. 

The update rate of Plotter is sufficient for live video productions of time-varying data. I get up to 30 Hz refresh rates in my live telemetry system, just performing PLOT of the data, which completely clears out the previous plot and draws the whole thing all over again with fresh data.

The code defines a PLOTTER-PANE which can act alone in a Window, or be used within your own Interface designs, including those with several simultaneous PLOTTER-PANE displays. It is a CAPI:PANE object like any other. PLOTTER-PANE is an augmentation of a CAPI:OUTPUT-PANE.

Ad-hoc plotting of data from the keyboard is as simple as:
```
(PLOT 'name xs ys :clear t :thick 2 :symbol :circle :plot-joined t)
```
where 'name is used to refer to a Window made on demand to plot the data in xs and ys. Any number of additional options can be added with keyword args, as shown here. The data in xs and ys can be any sequence of numbers (LIST, VECTOR). You can omit the xs, in which case some xs will be generated using an integer ordinal sequence against the provided ys data. Plots can be resized with edge and corner dragging, and autmatically scale line widths as the size changes.

The data is scanned for its extrema and axes are automatially generated, leaving 15% free space to keep them nice looking for publication, and not too crowded. Axis increments are carefully selected to be multiples of 1, 2, or 5, and not some arbitrary crappy increment so often seen. Axis labels are chosen to prevent overcrowding along the axis. And you can furnish your own labeling either with a collection of labels, or with a function that will generate a label given an abscissa. You can specify logarithmic scaling on either axis using :XLOG T and/or :YLOG T. And you can specify overt axis ranges with :XRANGE and :YRANGE. The ranges can even be reverse order, as in :XRANGE '(10 -10), which labels the axis in the opposite direction from usual practice.

Functions can be plotted using:
```
(FPLOT 'name '(-10 10)
     (lambda (x)
        (/ (sin x) x))
     :clear t
     :thick 2)
```
where a domain, `(-10 10), and function, follows the name of the window receiving the plot. Data are generated for a PLOT using a variable density algorithm to present smooth looking curves of the function. We have PARAMPLOT for parametric function plots, COMPLEX-PLOT for complex-plane plots, and many others. You can force an :ASPECT 1 for complex plane plotting to keep the visual scaling on X and Y the same.

There are many different styles of plotting, as demonstrated in the image above: function plots, line plots, point plots, histogram, filled vertical or horizontal bar charts, image plots for 2-D data, etc, etc... Plots done without :CLEAR T will be drawn on top of an existing plot. So the image above was formed from at least 5 separate PLOT styles on top of each other, plus an annotation added with DRAW-STRING. 

Positions for annotation strings can be indicated with an anchor position: :N, :S, :E, :W, :NE, :NW, :SE, :SW, and a location: (:DATA x y), (:FRAC x y), or (:PIX x y), for data units, fraction of plotview units, or direct pixel measurement. 

Legends can be added automatically from each PLOT in the image, and the legend box can be dragged to any convenient location on the image, or removed with the popup menu. Measurements can be made with the cursor, setting a reference point somewhere, then clicking on the plot to show the data value at that location and its dx and dy from the reference point. Full-width crosshairs can be toggled on or off from the popup menu.
