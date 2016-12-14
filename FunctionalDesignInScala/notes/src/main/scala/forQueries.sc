package main.scala

object forQueries {
	case class Books(title: String, authors: List[String])
	
	val books: List[Books] = List(
		Books(title = "Learning Scala",
			authors  = List("Author1", "Author2")
		),
		Books(title = "Advacned  Scala",
			authors  = List("Author1a", "Author2b")
		),
		Books(title = "Functional programming in Scala",
			authors  = List("FunAuthor1", "FunAuthor2")
		),
		Books(title = "Learning F#",
			authors  = List("Author1F", "Author2F")
		),
		Books(title = "F# Deep Dive",
			authors  = List("Author1D", "Author2D")
		),
		Books(title = "F# for fun and profit",
			authors  = List("WebAuthor1", "WebAuthor2")
		)
	)
	// exercise
	// res = List[String] = Lsit(Learning Scasa, Advanced Scala, Functional Programiming in Scala, Learning F#, F# Deep Dive, F# for fun and profit
	for {
		b <- books
		a <- b.authors
		if a contains "1"
		} yield b.title
		
	// translatation in HO functions
	// res = List[String] = Lsit(Learning Scasa, Advanced Scala, Functional Programiming in Scala, Learning F#, F# Deep Dive, F# for fun and profit
	books.flatMap(b => b.authors.withFilter(a => a.contains("1")).map(y => b.title))
	 // flatten all the authors in the Books "DB"
	// res: List[String] = List(Author1, Author2,......, WebAuthor2)
	books.flatMap(b => b.authors)
                                                  
	books.flatMap(b => b.authors).map(y => y.title)

// translatation in HO functions
	// res = ERROR:  value title is not a member of string
	// books.flatMap(b => b.authors.withFilter(a => a.contains("1")).map(b => b.title))
	// res = ERROR:  value title is not a member of string
	// books.flatMap(b => b.authors.withFilter(a => a.contains("1")).map(y => y.title))
}