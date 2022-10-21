# Make a scroll bar
makeScroll <- function(top){
  frame <- tkframe(top, class = "Scrollform")
  xbar <- tkscrollbar(frame, orient = "horizontal", command = function(...) tkxview(vport, ...))
  tkpack(xbar, side = "bottom", fill = "x")
  ybar <- tkscrollbar(frame, command = function(...) tkyview(vport, ...))
  tkpack(ybar, side = "right", fill = "y")
  vport <- tkcanvas(frame, xscrollcommand = function(...) tkset(xbar, ...), yscrollcommand = function(...) tkset(ybar, ...))
  tkpack(vport, side = "left", fill = "both", expand = TRUE)
  form <- tkframe(vport)
  tkcreate(vport, "window", "0 0", anchor = "nw", window = form$ID)
  tkbind(form, "<Configure>", function() resizeScroll(form))
  tkaddtag(vport, "iform", "all")
  iform.win <- tkitemconfigure(vport, tkfind(vport, "withtag", "iform"), "-window")
  return(frame)
}

# Make a scroll bar
resizeScroll <- function(form){
  stopifnot(tclvalue(tkwinfo("class", form)) == "Frame")
  vport <- tkwinfo("parent", form)
  stopifnot(tclvalue(tkwinfo("class", vport)) == "Canvas")
  bbox <- tkbbox(vport, "all")
  tkconfigure(vport, width = 800, height = 500, scrollregion = bbox, yscrollincrement = "0.1i")
}

# Make a scroll bar
getScroll <- function(sform){
  stopifnot(inherits(sform, "tkwin") && tclvalue(tkwinfo("class", sform)) == "Scrollform")
  children.ids <- unlist(strsplit(tclvalue(tkwinfo("children", sform)), " "))
  for(child.id in children.ids){
    if(tclvalue(tkwinfo("class", child.id)) == "Canvas"){
      vport <- .Tk.newwin(child.id)
      iform.win <- tkitemconfigure(vport, tkfind(vport, "withtag", "iform"), "-window")
      iform.id <- unlist(strsplit(tclvalue(iform.win), " "))[5]
      stopifnot(tclvalue(tkwinfo("class", iform.id)) == "Frame")
      form <- .Tk.newwin(iform.id)
      return(form)
    }
  }
}
