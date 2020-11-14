Racket Ray Tracer
=================

By Aaron Fritz

One of the things I like to do with languages I'm not very familiar with is to create a ray tracer with them. This normally satisfies my questions about 1) Their concurrency tools, 2) Their output tools in terms of graphics, and 3) Their overall performance. 

I do implement a similar, if not identical, ray tracing algorithm for each of my ray tracers, so the differences in performance should be somewhat comparable, despite various paradigm differences in each language, and so on. This ray tracer is not real-time by any means; however, I would say its performance is a bit faster than my Python ray tracer.

I do feel that it is relevant to have a ray tracer in a functional language like Racket. Ray tracers are conducive to immutable data structures, and that fits well with the functional paradigm, moreso than imperative programming languages like C or C#.

I have not yet implemented concurrency for this ray tracer in Racket, and I would like to figure that out sometime. Runs with DrRacket.
