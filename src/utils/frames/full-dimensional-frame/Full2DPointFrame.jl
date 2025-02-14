accessibles(fr::Full2DPointFrame, w::Point2D{Int}, ::_CL_N)  = Y(w) > Y(fr)-1 ? Point2D{Int}[] : IterTools.imap(y->Point2D{Int}(X(w), y), ((Y(w)+1):Y(fr)))
accessibles(fr::Full2DPointFrame, w::Point2D{Int}, ::_CL_S)  = Y(w) < 2       ? Point2D{Int}[] : IterTools.imap(y->Point2D{Int}(X(w), y), (1:(Y(w)-1)))
accessibles(fr::Full2DPointFrame, w::Point2D{Int}, ::_CL_E)  = X(w) > X(fr)-1 ? Point2D{Int}[] : IterTools.imap(x->Point2D{Int}(x, Y(w)), ((X(w)+1):X(fr)))
accessibles(fr::Full2DPointFrame, w::Point2D{Int}, ::_CL_W)  = X(w) < 2       ? Point2D{Int}[] : IterTools.imap(x->Point2D{Int}(x, Y(w)), (1:(X(w)-1)))

accessibles(fr::Full2DPointFrame, w::Point2D{Int}, ::_CL_NE) = Y(w) > Y(fr)-1 || X(w) > X(fr)-1 ? Point2D{Int}[] : IterTools.imap(Point2D{Int}, Iterators.product( ((X(w)+1):X(fr)), ((Y(w)+1):Y(fr)) ))
accessibles(fr::Full2DPointFrame, w::Point2D{Int}, ::_CL_NW) = Y(w) > Y(fr)-1 || X(w) < 2       ? Point2D{Int}[] : IterTools.imap(Point2D{Int}, Iterators.product( (1:(X(w)-1)),     ((Y(w)+1):Y(fr)) ))
accessibles(fr::Full2DPointFrame, w::Point2D{Int}, ::_CL_SE) = Y(w) < 2       || X(w) > X(fr)-1 ? Point2D{Int}[] : IterTools.imap(Point2D{Int}, Iterators.product( ((X(w)+1):X(fr)), (1:(Y(w)-1))     ))
accessibles(fr::Full2DPointFrame, w::Point2D{Int}, ::_CL_SW) = Y(w) < 2       || X(w) < 2       ? Point2D{Int}[] : IterTools.imap(Point2D{Int}, Iterators.product( (1:(X(w)-1)),     (1:(Y(w)-1))     ))
