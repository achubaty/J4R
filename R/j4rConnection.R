########################################################
# R functions for connection to Gateway Server in Java
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

#'
#' The current version of the J4R Java server
#'
#' @export
J4R_Server_Version <- "1.1.7"

#'
#' Connect to Java environment
#'
#' This function connects the R environment to a gateway server that runs in Java.
#'
#' The first argument of the function provides the listening port for the Java server. A maximum of four ports is
#' allowed. When set to 0, these ports are randomly selected. By default, the server listens to two random
#' ports.
#'
#' The extensionPath can either be set in this function or dynamically changed (see the addToClassPath function).
#' However, dynamic classpath changes are not allowed in Java version later than 16.
#'
#' The headless mode assumes the JVM has no keyboard, display or mouse. In order to enable
#' the UI on the Java end, the headless argument should be set to false.
#'
#' @param host the URL or IP address of the host ("localhost" by default )
#' @param port a vector of the listening ports for the Java server
#' @param extensionPath a vector of characters that contains the paths to jar files
#'  or to the classes that are to be loaded by the system classloader.
#' @param memorySize the memory size of the Java Virtual Machine in Mb (if not specified, the JVM runs with the default memory size)
#' @param public true to connect to a server that is already running locally (FALSE by default)
#' @param internalPort a vector of two integers representing the backdoor port and the garbage collector port
#' @param key an integer used as a token to ensure a secure connection
#' @param headless a boolean to enable the headless mode (is true by default).
#'
#' @seealso \code{\link{addToClassPath}}
#'
#' @return a logical TRUE if the function managed to get connected to the server or if it was already connected or
#' FALSE if the connection has failed
#'
#' @export
connectToJava <- function(host = "localhost",
                          port = c(0,0),
                          extensionPath = NULL,
                          memorySize = NULL,
                          public = FALSE,
                          internalPort = c(0,0),
                          key = NULL,
                          headless = T) {
  if (isConnectedToJava()) {
    message("It seems R is already connected to the local Java server.")
    return(TRUE)
  } else {
    if (public) {
      if (is.null(port)) {
        stop("The port argument cannot be null in public mode. Please use the ports you specified when you started the local server!")
      }
      assign("connectionHandler", J4RConnectionHandler(host, port, key, internalPort), envir = cacheEnv, inherits = F)
    } else {
      if (.isVerbose()) {
        message(.checkJavaVersionRequirement())
      }
      message("Starting local Java server...")
      parms <- c("-firstcall", "true")

      if (!is.null(port)) {
        if (any(port < 0)) {
          stop("Ports should be integers equal to or greater than 0!")
        }
        if (length(port) > 4) {
          stop("J4R allows for a maximum of 4 ports!")
        }
        parms <- c(parms, "-ports", paste(port,collapse=portSplitter))
      }

      if (!is.null(internalPort)) {
        if (any(internalPort < 0)) {
          stop("Internal ports should be integers equal to or greater than 0!")
        }
        if (length(internalPort) != 2) {
          stop("There should be two internal ports!")
        }
        parms <- c(parms, "-backdoorport", paste(internalPort,collapse=portSplitter))
      }

      if (!is.null(extensionPath)) {
        quotedExtensionPath <- paste("\"", extensionPath, "\"", sep="")
        parms <- c(parms, "-ext", paste(quotedExtensionPath, collapse = "::"))
      }

      if (!is.null(memorySize)) {
        if (!is.numeric(memorySize) && !is.integer(memorySize)) {
          stop("The memorySize parameter should be either a numeric or an integer!")
        }
        if (memorySize < 50) {
          stop("The minimum memory for the JVM is 50 Mb!")
        }
        parms <- c(parms, "-mem", as.integer(memorySize))
      } else if (exists("defaultJVMMemory", envir = settingEnv, inherits = F)) {
        memorySize <- get("defaultJVMMemory", envir = settingEnv, inherits = F)
        parms <- c(parms, "-mem", as.integer(memorySize))
      }

      if (headless)
        parms <- c(parms, "-headless", "true")
      else
        parms <- c(parms, "-headless", "false")

      parms <- c(parms, "-wd", paste("\"",getwd(),"\"", sep=""))
      filename <- file.path(getwd(), "J4RTmpFile")
      if (file.exists(filename)) {
        file.remove(filename)
      }

      rootPath <- system.file("java", package = "J4R")
#    message(rootPath)
      architecture <- suppressMessages(getJavaVersion()$architecture)
      if (architecture == "32-Bit") {
        stop("Java 32-Bit version are no longer supported!")
      } else {
        jarFilename <- paste("j4r_server-", J4R_Server_Version, ".jar", sep="")
      }
      path <- normalizePath(file.path(rootPath, jarFilename))
      if (!file.exists(path)) {
        stop("The path to the j4rserver library is incorrect!")
      }
      quotedPath <- paste("\"", path, "\"", sep="")
      returnCode <- system2(.getJavaPath(), args = c("-Xmx50m", "-Djava.awt.headless=true", "-jar", quotedPath, parms), wait = F)  ### first JVM should always use the -Djava.awt.headless=true option. Otherwise headless JVM will fail to load the first JVM.
      if (returnCode != 0) {
        stop("The call to the system2 function has returned an exception!")
      }
      initialTime <- Sys.time()
      while (!file.exists(filename)) {
        Sys.sleep(0.1)
        elapsedTime <- Sys.time() - initialTime
        if (elapsedTime > 8) {
          stop("It seems the local Java server has failed to start!")
        }
      }
      initialTime <- Sys.time()
      while (file.exists(paste(filename, ".lock", sep=""))) {
        Sys.sleep(0.1)
        elapsedTime <- Sys.time() - initialTime
        if (elapsedTime > 2) {
          stop("It seems the lock on the info file has not been lifted!")
        }
      }
      .instantiateConnectionHandlerFromFile()
    }
    isSecure <- .createAndSecureConnection()
    if (!isSecure) {
       .internalShutdown()  ### to make sure the connectionHandler is removed
    }
    return(isSecure)
  }
}

.instantiateConnectionHandlerFromFile <- function() {
  info <- suppressWarnings(utils::read.table("J4RTmpFile", header=F, sep=";", stringsAsFactors = F))
  key <- as.integer(info[1,1])
  internalports <- as.integer(strsplit(info[1,2], split = portSplitter)[[1]])
  if (is.integer(info[1,3])) { ### happens with a single port
    port <- as.integer(info[1,3])
  } else {
    port <- as.integer(strsplit(info[1,3], split = portSplitter)[[1]])
  }
  assign("connectionHandler", J4RConnectionHandler("localhost", port, key, internalports), envir = cacheEnv, inherits = F)  ### instantiated in the context of a private server
}




#'
#' Checks if the Java server is running
#'
#' This is done by checking f the socket connection to the JVM exists.
#'
#' @return a logical
#'
#' @export
isConnectedToJava <- function() {
  if (exists("connectionHandler", envir = cacheEnv, inherits = F)) {
    stillConnected <- .isThereAtLeastOneConnection(get("connectionHandler", envir = cacheEnv, inherits = F))
    return(stillConnected)
  } else {
    return(FALSE)
  }
}

#'
#' The number of connections to the server
#'
#' @return the number of sockets connected to the server
#'
#' @export
getNbConnections <- function() {
  if (exists("connectionHandler", envir = cacheEnv, inherits = F)) {
    return(length(get("connectionHandler", envir = cacheEnv, inherits = F)$connections))
  } else {
    return(0)
  }
}


#'
#' Shut down R client
#'
#' This function shuts down the client. If the server is private, it is also shut down.
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
shutdownClient <- function() {
  .softExit()
}

.internalShutdown <- function() {
  if (isConnectedToJava()) {
    for (aff in 1:getNbConnections()) {
      socket <- .getSocket(affinity = aff)
      if (!is.null(socket)) {
        utils::write.socket(socket, "closeConnection")
        utils::close.socket(socket)
      }
    }
    gcSocket <- .getGCSocket()
    if (!is.null(gcSocket)) {
      utils::write.socket(gcSocket, "closeConnection")
      utils::close.socket(gcSocket)
    }
    message("Closing connections and removing sockets...")
  }
  if (exists("connectionHandler", envir = cacheEnv, inherits = F)) {  # when security is not validated, the connectionhandler object remains
    rm("connectionHandler", envir = cacheEnv)
  }
  if (exists("dumpPile", envir = cacheEnv, inherits = F)) {
    rm("dumpPile", envir = cacheEnv)
  }
  if (exists("classMap", envir = cacheEnv, inherits = F)) {
    rm("classMap", envir = cacheEnv)
  }
  listOfJavaReferences <- getListOfJavaReferences()
  if (!is.null(listOfJavaReferences) && length(listOfJavaReferences) > 0) {
    rm(list = listOfJavaReferences, envir = .GlobalEnv)
  }
}

.removeTmpFile <- function() {
  filename <- file.path(getwd(), "J4RTmpFile")
  if (file.exists(filename)) {
    file.remove(filename)
  }
}


#'
#' Shut down Java
#'
#' This function shuts down Java and the gateway server. THIS FUNCTION IS DEPRECATED.
#' PLEASE USE shutdownClient instead
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
shutdownJava <- function() {
  .Deprecated("shutdownClient")
  .softExit()
}


#'
#' Synchronize the Java environment with the R environment
#'
#' This function call the garbage collector in R and sends the list of Java references
#' that have been collected to the Java server. These references are then removed from
#' the internal map.
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
callJavaGC <- function() {
  gc()
  .flushDumpPileIfNeeded(1)
}


.flush <- function(javaList) {
  prefix <- "flush"
  maxLength <- length(javaList)
  nbCalls <- ceiling(maxLength / maxVectorLength)
  output <- NULL
  for (i in 1:nbCalls) {
    lowerIndex <- (i-1) * maxVectorLength + 1
    upperIndex <- i * maxVectorLength
    if (upperIndex > maxLength) {
      upperIndex <- maxLength
    }
    subList <- javaList[lowerIndex:upperIndex]
    subcommands <- paste("java.object",.translateJavaObject(subList),sep="")
    command <- paste(prefix, subcommands, sep=MainSplitter)
    utils::write.socket(.getGCSocket(), command)
    callback <- utils::read.socket(.getGCSocket(), maxlen=bufferLength)
    output <- .processResult(callback, output)
  }
  if (!is.null(output)) {
    stop("The local Java server has returned something else than NULL!")
    return(output)
  } else {
    return(invisible(output))
  }
}

#' Return the number of instances stored in the
#' internal map of the Java server
#'
#' @return an integer
#'
#' @export
getNbInstancesInInternalMap <- function() {
  command <- "size"
  utils::write.socket(.getSocket(), command)
  callback <- utils::read.socket(.getSocket(), maxlen=bufferLength)
  return(.processCallback(callback))
}

#'
#' Retrieve the paths of the current classloader
#'
#' This functions returns the paths that are currently included
#' in the System classloader.
#'
#' @export
getClassLoaderPaths <- function() {
  paths <- callJavaMethod("j4r.lang.J4RSystem", "getClassPathURLs")
  pathsList <- getAllValuesFromListObject(paths)
  return(pathsList)
}

#'
#' Retrieve the URLs of the current classloader
#'
#' This function returns the URLs that are currently included
#' in the System classloader.
#'
#' This function is deprecated. Please use the getClassLoaderPaths instead.
#'
#' @export
getClassLoaderURLs <- function() {
  .Deprecated("getClassLoaderPaths")
  return(getClassLoaderPaths())
}


#'
#' Check if a Library has been loaded
#'
#' It checks if a particular library is part of the classpath.
#'
#' @param myJavaLibrary a character string that stands for the java library (e.g. repicea.jar)
#'
#' @export
checkIfClasspathContains <- function(myJavaLibrary) {
  if (isConnectedToJava()) {
    listURLs <- getClassLoaderPaths()
    isLibIn <- FALSE
    if (length(listURLs) > 1) {
      for (i in 1:length(listURLs)) {
        if (grepl(myJavaLibrary, listURLs[i])) {
          isLibIn <- TRUE
          break
        }
      }
    } else {
      isLibIn <- grepl(myJavaLibrary, listURLs)
    }
    return(isLibIn)
  } else {
    message("The local Java server is not running.")
  }
}

.getLibraryPath <- function(packageName, myJavaLibrary) {
  filename <- system.file("inst", myJavaLibrary, package = packageName)
  if (file.exists(filename)) {  ### test mode
    filePath <- filename
  } else {
    filename <- system.file(myJavaLibrary, package = packageName)
    if (file.exists(filename)) {  ### normal mode
      filePath <- filename
    } else {
      filePath <- NULL
    }
  }
  return(filePath)
}

#'
#' Force the JVM to shut down
#'
#' This is the not so gentle way to exit the JVM.
#'
#' In case the JVM is stuck and does not respond to interrupt. It is possible
#' to force the shutdown through this function.
#'
#' @export
killJava <- function() {
  tryCatch(
    {
      emergencySocket <- .getBackdoorSocket()
      utils::write.socket(emergencySocket, "emergencyShutdown")
      invisible(utils::close.socket(emergencySocket))
    },
    error=function(cond) {
      message("Unable to connect the Java server. It might be already down!")
    }
  )
  .internalShutdown()
  .removeTmpFile()
  Sys.sleep(2)  ### wait two seconds to make sure the server is really shut down
  message("Done.")
}



#'
#' Return the main instance in the case of a public server
#'
#' An instance of a particular class can be associated to a public server. The approach
#' is similar to that of Py4j package, where the gateway server is just a channel to get
#' to a particular instance. This R function retrieves this instance.
#'
#' @return a java.object instance or null if the main instance was not set
#'
#' @export
getMainInstance <- function() {
  return(callJavaMethod("j4r.net.server.JavaGatewayServer", "getMainInstance"))
}



#'
#' Interrupt the current task on the Java server
#'
#'
#' @export
interruptJava <- function() {
  tryCatch(
    {
      emergencySocket <- .getBackdoorSocket()
      utils::write.socket(emergencySocket, "interrupt")
      invisible(utils::close.socket(emergencySocket))
    },
    error=function(cond) {
      message("Unable to connect to the local Java server. It might be already down!")
    }
  )
}


.softExit <- function() {
  tryCatch(
    {
      emergencySocket <- .getBackdoorSocket()
      utils::write.socket(emergencySocket, "softExit")
      invisible(utils::close.socket(emergencySocket))
    },
    error=function(cond) {
      message("Unable to connect the local Java server. It might be already down!")
    }
  )
  .internalShutdown()
  .removeTmpFile()
  Sys.sleep(2)  ### wait two seconds to make sure the server is really shut down
  message("Done.")
}


#'
#' Dynamically adds a path or a jar file to the classpath.
#'
#' This function makes it possible to add a directory or a JAR file
#' to the class path. If the packageName parameter is null then the urlString
#' parameter must be the complete path to the directory. Otherwise, it can be
#' the name of the JAR file and the function will find the path through the package
#' name. A non null packageName parameter is typically used in packages that rely
#' on J4R. IMPORTANT This function is not compatible with Java 16 and later.
#'
#' @param path a character representing the path to the directory or the JAR file
#' if the packageName parameter is null. Otherwise, it can just be the name of the JAR file. This path
#' is normalized so that expressions like myJar.jar or ./extensions/myJar.jar will be processed.
#' @param packageName a character representing the package.
#'
#' @export
addToClassPath <- function(path, packageName = NULL) {
  if (isConnectedToJava()) {
    if (!is.null(packageName)) {
      path <- .getLibraryPath(packageName, path)
    }
    callJavaMethod("j4r.lang.J4RSystem", "addToClassPath", normalizePath(path))
  } else {
    message("The local Java server is not running.")
  }
}


#'
#' Dynamically adds an url to the classpath.
#'
#' This function makes it possible to add a directory or a JAR file
#' to the class path. If the packageName parameter is null then the urlString
#' parameter must be the complete path to the directory. Otherwise, it can be
#' the name of the JAR file and the function will find the path through the package
#' name. A non null packageName parameter is typically used in packages that rely
#' on J4R.
#'
#' This function is deprecated. Use the addToClassPath function instead.
#'
#' @param urlString a character representing the complete path to the directory or the JAR file
#' if the packageName parameter is null. Otherwise, it can just be the name of the JAR file.
#' @param packageName a character representing the package.
#'
#' @export
addUrlToClassPath <- function(urlString, packageName = NULL) {
  .Deprecated("addToClassPath")
  addToClassPath(urlString, packageName)
}


