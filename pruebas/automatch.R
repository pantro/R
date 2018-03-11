go = function(maximumLevels=5, maxStrLen=25, terminalWidth=125) {
  options(width=terminalWidth)
  while(TRUE) {
    if (compareData(maxLevels=maximumLevels, maxStrLen=maxStrLen)) {
      quit(ifelse(file.exists("debug.txt"), "ask", "no"))
    }
  }
}

compareData = function(maxLevels=5, maxStrLen=25) {
  # Setup
  library(utils)
  options(locatorBell = FALSE)
  on.exit(flush.console())
  exit = function(preamble=NULL) {
    stockMessage = "Choose 'OK' to compare a new pair or 'Cancel' to quit altogether."
    message = ifelse(is.null(preamble), stockMessage, paste(preamble, stockMessage, sep="\n"))
    ans = utils:::winDialog(type="okcancel", message=message)
    return(ifelse(is.null(ans) || ans=="CANCEL", 1, 0))
  }
  
  # Function to read files
  fileTypes = cbind(c("Excel","Comma-separate values","SPSS"), c("*.xls;*.xlsx","*.csv","*.sav"))
  getFile = function(id="master", baseType="*.xls;*.xlsx") {
    index = match(baseType, fileTypes[,2])
    if (is.na(index)) return(paste("getFile() passed invalid extension:", baseType))
    fname = utils::choose.files(caption=paste("Select", id, "file"), multi=FALSE, 
                                filters=fileTypes, index=index)
    if (fname=="") return(paste("No", id, "file chosen."))
    Sel = regmatches(fname, regexpr("([.]xls|[.]xlsx|[.]csv|[.]sav)$", fname))
    ext = fileTypes[grep(Sel, fileTypes[,2]),2]
    
    # Read file according to kind of file
    if (ext=="*.sav") {
      library(foreign)
      dtf = suppressWarnings(try(foreign::read.spss(fname, to.data.frame=TRUE)))
      if (is(dtf, "try-error"))
        return(paste(id, "does not appear to be a valid spss file."))
    } else if (ext=="*.csv") {
      dtf = try(read.csv(fname, as.is=TRUE))
      if (is(dtf, "try-error")) return(paste(id, "does not appear to be a valid csv file."))
    } else if (ext=="*.xls;*.xlsx") {
      temp = grep("PERL", toupper(strsplit(Sys.getenv()[["PATH"]], ";")[[1]]))
      if (length(temp)==0) return("Please install ActivePerl from the web and try again.")
      temp = require(gdata, quietly=TRUE, warn.conflicts=FALSE)
      if (!temp) return("Install R package 'gdata', then try again.")
      dtf = try(gdata::read.xls(fname, as.is=TRUE))
      if (is(dtf, "try-error")) {
        return(paste(id, " does not appear to be a valid xls file."))
      } 
    } else {
      return(paste("Unknown file type:", ext))
    }
    
    return(list(fileName=fname, dtf=dtf, extension=ext))
  }
  
  # Input master file
  master = getFile(id="master", baseType="*.xls;*.xlsx")
  if (is.character(master)) return(exit(master))
  m = master$dtf
  mext = master$extension
  master = master$fileName
  
  # Input slave file
  slave = getFile(id="slave", baseType=mext)
  if (is.character(slave)) return(exit(slave))
  s = slave$dtf
  slave = slave$fileName
  if (master==slave) return(exit("Master and slave cannot be the same file."))
  
  # Check variable names (this function does not handle duplicate variable names)
  mFields = names(m)
  if (any(duplicated(mFields))) 
    return(exit("In master: cannot handle duplicated column names."))
  if (any(duplicated(toupper(mFields)))) 
    return(exit("In master: cannot handle column names differing only in case."))
  sFields = names(s)
  if (any(duplicated(sFields))) 
    return(exit("In slave: cannot handle duplicated column names."))
  if (any(duplicated(toupper(sFields)))) 
    return(exit("In slave: cannot handle column names differing only in case."))
  
  # Report file names
  msgs = paste("Master: ", master, "\n", "Slave: ", slave, "\n\n", sep="")
  cat(msgs)
  
  # Correct case mismatch in variable names
  cFields = intersect(mFields, sFields)
  temp = sapply(sFields, 
                function(f) is.na(match(f,mFields)) &&
                  !is.na(match(toupper(f),toupper(mFields))))
  if (any(temp)) {
    for (i in which(temp)) {
      mName = names(m)[match(toupper(sFields[i]), toupper(mFields))]
      msg = paste("Changing ", sFields[i], " in slave to ", mName, ".\n", sep="")
      msgs = c(msgs, msg)
      cat(msg)
      names(s)[i] = mName
      cFields = c(cFields, mName)
      names(s)[match(toupper(mName),toupper(names(s)))] = mName
    }
    sFields = names(s)
    cat("\n")
    msgs = c(msgs, "\n")
  }
  
  # Add case-mismatched fields to 'cFields' and restore to master order
  if (length(cFields)==0) return(exit("No variable names in common."))
  temp = mFields[mFields %in% intersect(mFields,cFields)]
  temp2 = match(cFields, temp)
  cFields = cFields[order(temp2)]
  
  # Report on variable differences
  temp = setdiff(mFields, cFields)
  if (length(temp)>0) {
    msg = "Note: The following variables are in 'master' but not 'slave':\n"
    msgs = c(msgs, msg)
    cat(msg)
    for (i in 1:length(temp)) {
      msg = paste(temp[i],"\n")
      msgs = c(msgs, msg)
      cat(msg)
    }
    flush.console()
  }
  temp = setdiff(sFields, cFields)
  if (length(temp)>0) {
    msg = "Note: The following variables are in 'slave' but not 'master':\n"
    msgs = c(msgs, msg)
    cat(msg)
    for (i in 1:length(temp)) {
      msg = paste(temp[i],"\n")
      msgs = c(msgs, msg)
      cat(msg)
    }
    flush.console()
  }
  if (length(sFields)==length(mFields) && length(sFields)==length(cFields)) {
    msg = "All variable names match.\n"
    msgs = c(msgs, msg)
    cat(msg)
  }
  
  mTypes = sapply(m[,cFields], class)
  mTypes[mTypes=="integer"] = "numeric"
  sTypes = sapply(s[,cFields], class)
  sTypes[sTypes=="integer"] = "numeric"
  if (all(mTypes==sTypes)) {
    msg = "All variable types agree.\n"
    msgs = c(msgs, msg)
    cat(msg)
  } else {
    Sel = which(mTypes!=sTypes)
    for (i in Sel) {
      msg = paste(cFields[i], " is ", mTypes[i], " in master and ",
                  sTypes[i], " in the slave.\n")
      msgs = c(msgs, msg)
      cat(msg)
    }
    cat("\n")
    msgs = c(msgs, "\n")
  }
  
  # Remove blank rows
  mBlank = apply(m, 1, function(r) all(sapply(r,is.na)))
  if (any(mBlank)) {
    msg = paste("Removing", sum(mBlank), " blank rows from master.\n")
    msgs = c(msgs, msg)
    cat(msg)
    m = m[!mBlank,]
  }
  sBlank = apply(s, 1, function(r) all(sapply(r,is.na)))
  if (any(sBlank)) {
    msg = paste("Removing", sum(sBlank), " blank rows from slave.\n")
    msgs = c(msgs, msg)
    cat(msg)
    s = s[!sBlank,]
  }
  
  # Compare numbers of rows
  nMax = nrow(m)
  if (nrow(m)>nrow(s)) {
    msg = paste("'master' has ", nrow(m), " rows, but 'slave' only has ", nrow(s),".\n\n")
    msgs = c(msgs, msg)
    cat(msg)
    nMax = nrow(s)
  } else if (nrow(s)>nrow(m)) {
    msg = paste("'slave' has ", nrow(s), " rows, but 'master' only has ", nrow(m),".\n\n")
    msgs = c(msgs, msg)
    cat(msg)
  } else {
    msg = paste("Master and slave each have", nrow(m), "rows.\n\n")
    msgs = c(msgs, msg)
    cat(msg)
  }
  if (nrow(m)==0) return(exit("No rows to analyze in master."))
  if (nrow(s)==0) return(exit("No rows to analyze in slave."))
  
  flush.console()
  ctype = utils::menu(c("Compare by id","Compare by line number"), graphics=TRUE,
                      title="Choose comparison type:")
  if (ctype==0) return(exit("Cancelled by user."))
  if (ctype==1) {
    temp = cFields
    temp2 = "Select ID column:"
    tempX=temp; tempX[1] = paste(temp[1], gsub(".", " ", temp2))
    id = utils::menu(tempX, graphics=TRUE, title=temp2)
    if (id==0) return(exit("Cancelled by user."))
    id = temp[id]
    byLine = FALSE
  } else {
    id = NULL
    byLine = TRUE
  }
  
  # check if rows can be compared by id
  if (!is.null(id)) {
    if (any(duplicated(m[,id]))) {
      byLine = TRUE
      msg = paste("'", id, 
                  "' is not unique in master -- rows will be compared sequentially\n", sep="")
      msgs = c(msgs, msg)
      cat(msg)
    }
    if (any(duplicated(s[,id]))) {
      byLine = TRUE
      msg = paste("'", id, 
                  "' is not unique in slave -- rows will be compared sequentially\n", sep="")
      msgs = c(msgs, msg)
      cat(msg)
    }
    if (byLine) {
      msg = "\n"
      msgs = c(msgs, msg)
      cat(msg)
    } else {
      mTF = m[,id] %in% s[,id]
      mSel = which(mTF)
      if (any(!mTF)) {
        temp = paste("'", id, "' in master but not slave: ",
                     paste(m[!mTF,id], collapse=", "), sep="")
        msg = c(strwrap(temp, options("width")$width), "\n")
        msgs = c(msgs, msg)
        cat(msg)
      }
      sExtra = is.na(match(s[,id], m[mSel,id]))
      if (any(sExtra)) {
        temp = paste("'", id, "' in slave but not master: ",
                     paste(s[sExtra,id], collapse=", "), sep="")
        msg = c(strwrap(temp, options("width")$width), "\n")
        msgs = c(msgs, msg)
        cat(msg)
      }
      if (any(mTF) || any(sExtra)) {
        msgs = c(msgs, "\n")
        cat("\n")
      }
      sSelOrd = match(m[mSel,id], s[,id])
    }
  }
  
  
  # setup where to report differences
  outFun = function(expr, out, append=TRUE) {
    if (is.null(out)) {
      if (is.data.frame(expr)) {
        print(expr, row.names=FALSE)
      } else if (is.character(expr)) {
        for (i in 1:length(expr)) cat(gsub("\\n$","",expr[i]),"\n")
      } else {
        print(expr)
      }
    } else {
      if (is.character(expr)) {
        temp = capture.output(cat(expr, sep=""))
      } else {
        temp = capture.output(expr)
      }
      write(temp, file=out, append = append)
    }
    invisible(NULL)
  }
  Sel = regexpr("[.][[:alpha:]]*$", master)
  out = paste(substring(master, 1, Sel-1), "-Compare.txt", sep="")
  ans = utils::menu(c("Screen", out), graphics=TRUE, title="Choose results location")
  if (ans==0) return(exit("Cancelled by user."))
  if (ans==1) {
    out = NULL
  } else {
    outFun(msgs, out, append=FALSE)
  }
  
  # report differences
  if (byLine) {
    lineIds = 1:nMax
  } else {
    lineIds = m[mSel,id]
  }
  upTrim = function(x) toupper(sub("^[ ]+", "", sub("[ ]+$", "", x)))
  for (col in 1:length(cFields)) {
    var = cFields[col]
    outFun(paste("Report for '", cFields[col], "':\n", sep=""), out)
    mDat = m[,var]
    if (is.factor(mDat)) mDat = levels(mDat)[as.numeric(mDat)]
    if (is.character(mDat) && max(nchar(mDat)) > maxStrLen) {
      outFun("Skipped (assuming this is a comment field)\n", out)
      next
    }
    tab = table(mDat, exclude=NULL)
    levelsSeen = length(tab) - 1
    if (is.character(mDat) || levelsSeen <= maxLevels) {
      if (levelsSeen <= maxLevels) {
        outFun("Master frequencies:\n", out)
      } else {
        outFun(paste("Master frequencies for the first", maxLevels, "levels out of", 
                     levelsSeen, "levels:\n"), out)
      }
      outFun(c(tab[1:min(levelsSeen,maxLevels)],tab[length(tab)]), out)
    } else {
      outFun("Master statistics:\n", out)
      outFun(paste("min=", min(mDat,na.rm=TRUE), "  max=", max(mDat,na.rm=TRUE),
                   "  mean=", mean(mDat,na.rm=TRUE), "  #NA=", sum(is.na(mDat)), "\n", sep=""),
             out)
    }
    if (byLine) {
      mDat = mDat[1:nMax]
      sDat = s[1:nMax,var]
      if (is.factor(sDat)) sDat = levels(sDat)[as.numeric(sDat)]
    } else {
      mDat = mDat[mSel]
      sDat = s[sSelOrd, var]
      if (is.factor(sDat)) sDat = levels(sDat)[as.numeric(sDat)]
    }
    Sel = (is.na(mDat) & is.na(sDat)) | (!is.na(mDat) & !is.na(sDat) & mDat == sDat)
    if (all(Sel)) {
      outFun("All values agree.\n", out)
    } else {
      outFun(paste("Disagreements for '", cFields[col], "':\n", sep=""), out)
      if (is.null(id)) {
        err = data.frame(line=lineIds[!Sel], master=mDat[!Sel], slave=sDat[!Sel])
      } else {
        err = data.frame(x=lineIds[!Sel], master=mDat[!Sel], slave=sDat[!Sel])
        names(err)[1] = id
      }
      if (is.character(mDat)) {
        subSel = !is.na(mDat[!Sel]) & !is.na(mDat[!Sel])
        onlyCaseSpaceDiff = rep("", sum(!Sel))
        onlyCaseSpaceDiff[subSel] = c("","*")[1+(upTrim(mDat[!Sel][subSel])==upTrim(sDat[!Sel][subSel]))]
        err = cbind(err, onlyCaseSpaceDiff)
      }
      outFun(err, out)
    }
    outFun("\n", out)
  }
  
  return(exit("Successfully compared the files."))
}

.First = function() go()

# Instructions: Load this file into R.  Quit, saveing image.  Restart to run program automatically.
# To edit: create 'debug.txt' in the working directory.

save.image()
go()