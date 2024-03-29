% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure.R
\name{measure_base}
\alias{measure_base}
\title{Master class for aggregate items}
\description{
When creating aggregate items, be sure to inherit from and implement this class.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{ret}}{return value}

\item{\code{name}}{using column name}

\item{\code{weight}}{using weight's column name}

\item{\code{count}}{data count}

\item{\code{sum}}{sum data}

\item{\code{min}}{min in the data}

\item{\code{max}}{max in the data}

\item{\code{top1}}{top1 value in the data}

\item{\code{top2}}{top2 value in the data}

\item{\code{rate}}{Percentage of top1 in sum}

\item{\code{added}}{is added}

\item{\code{selection}}{using all column names.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-measure_base-init}{\code{measure_base$init()}}
\item \href{#method-measure_base-set_weight}{\code{measure_base$set_weight()}}
\item \href{#method-measure_base-info}{\code{measure_base$info()}}
\item \href{#method-measure_base-calc}{\code{measure_base$calc()}}
\item \href{#method-measure_base-add}{\code{measure_base$add()}}
\item \href{#method-measure_base-clone}{\code{measure_base$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-measure_base-init"></a>}}
\if{latex}{\out{\hypertarget{method-measure_base-init}{}}}
\subsection{Method \code{init()}}{
Initialize internal variables.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{measure_base$init()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-measure_base-set_weight"></a>}}
\if{latex}{\out{\hypertarget{method-measure_base-set_weight}{}}}
\subsection{Method \code{set_weight()}}{
set using weight name.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{measure_base$set_weight(weight)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{weight}}{using weight name.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-measure_base-info"></a>}}
\if{latex}{\out{\hypertarget{method-measure_base-info}{}}}
\subsection{Method \code{info()}}{
list of internal variables.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{measure_base$info()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
variables list
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-measure_base-calc"></a>}}
\if{latex}{\out{\hypertarget{method-measure_base-calc}{}}}
\subsection{Method \code{calc()}}{
calculation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{measure_base$calc(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{A measure class}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-measure_base-add"></a>}}
\if{latex}{\out{\hypertarget{method-measure_base-add}{}}}
\subsection{Method \code{add()}}{
Add up the data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{measure_base$add(target)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{target}}{A measure class}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-measure_base-clone"></a>}}
\if{latex}{\out{\hypertarget{method-measure_base-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{measure_base$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
