*!Version 2.2 - 13.12.2023
*!Created by Jan Ditzen - jan.ditzen@unibz.it - jan.ditzen.net
*Changelog:
*       Version 2.0:
*       10.11.2016 - fmt option changed. now possible to set fmt for each column.
*       05.10.2016 - coeff option and m_signficance program added
*       Version 1.0:
*       02.03.2015 - Error in append/replace fixed
*       28.06.2015 - Fixed error in substitute
*       05.07.2015 - Added program force_override which pauses the program for 0.5seconds and tries to write file again if error is 603 (file can not be opened).
*       11.07.2015 - Added option coldelimiter, rowdelimiter, show
*       20.07.2015 - Fixed error on preheader option.
*       29.06.2015 - Added version 10
*       02.11.2020 - added option sleep
*       Version 2.2
*       13.12.2022 - added check for filetype and fixed bug when path had empty spaces. Now sets directory.

capture program drop mmat2tex
program define mmat2tex
        syntax anything using/ [, replace append fmt(string ) preheader(string)  colnames(string asis) postheader(string)  /*
                */ bottom(string) rownames(string asis) substitute(string asis) insertendrow(string asis) coldelimiter(string) /*
				*/ rowdelimiter(string) show  COEFFicient(string asis) COEFFicients sleep(real 500) NOCDSET ]
        tempname file mataoutput rownamesmatrix colnamesmatrix

        version 10

        if "`replace'" != "" & "`append'" != "" {
                display "Too many options, either replace or append allowed."
                exit
        }
        if "`replace'" != "" {
                local replaceappend replace
        }
        if "`append'" != "" {
                local replaceappend append
        }
        if "`replace'" == "" & "`append'" == "" {
                local replaceappend replace
        }

        if "`fmt'" == "" {
                local fmt "%12.4f"
        }
        if "`coldelimiter'" == "" {
                local coldelimiter "&"
        }
        if "`rowdelimiter'" == "" {
                local rowdelimiter "\\"
        }

        if "`nocdset'" == "" {
                tempname mfile mpath
                qui mata pathsplit("`using'",`mpath'="",`mfile'="")

                mata st_local("suffix",pathsuffix(`mfile'))
                
                if "`suffix'" == "" {
                        mata `mfile'=`mfile'+".tex"      
                }
                else if "`suffix'" != ".tex" {
                        mata `mfile' = subinstr(`mfile',"`suffix'",".tex")
                }

                mata st_local("using",`mfile')
                mata st_local("cdir_new",`mpath')

                local cdir_old "`c(dir)'"
                qui cd "`cdir_new'"
        }
	mata: `mataoutput' = m_convert_matrix(`anything',"`fmt'")

	if "`coefficient'`coefficients'" != "" {
	       if "`coefficients'" == "coefficients" {
			mata `mataoutput' = m_significance(`mataoutput',(0.1,0.05,0.01),999,1,1)
		}
		else {
			mmat2tex_coefficient `mataoutput' , `coefficient'
			mata `mataoutput'[(`r(ux)'..`r(lx)'),(`r(uy)'..`r(ly)')] = m_significance(`mataoutput'[(`r(ux)'..`r(lx)'),(`r(uy)'..`r(ly)')],(`r(levels)'),`r(dof)',`r(par)',`r(star)')
		}
	}

	//add columns
	mata: `mataoutput' = m_add_cols(`mataoutput',"`coldelimiter'","`rowdelimiter'")

        if `"`rownames'"' != "" {
                mata: `rownamesmatrix' = strofreal(J(rows(`mataoutput'),2,0))
                local i = 1
                foreach s in `rownames' {
                        mata: `rownamesmatrix'[`i',1] = ("`s'")
                        mata: `rownamesmatrix'[`i',2] = ("&")
                        local i= `i' + 1
                }
        }

        if `"`colnames'"' != "" {
                mata: `colnamesmatrix' = strofreal(J(1,cols(`mataoutput'),0))
                local i = 1
                foreach s in `colnames' {
                        mata: `colnamesmatrix'[1,`i'] = ("`s'")
                        mata: `colnamesmatrix'[1,`i'+1] = ("&")
                        local i= `i' + 2
                }
                mata: `colnamesmatrix'[1,cols(`mataoutput')] = ("\\")
        }
        if `"`colnames'"' != "" & `"`rownames'"' != "" {
                mata: `mataoutput' = (("" , "&" , `colnamesmatrix' ) \ (`rownamesmatrix' , `mataoutput'))
        }

        if `"`colnames'"' != "" & `"`rownames'"' == "" {
                mata: `mataoutput' = (`colnamesmatrix' \ `mataoutput')
        }

        if `"`colnames'"' == "" & `"`rownames'"' != "" {
                mata: `mataoutput' = (`rownamesmatrix' , `mataoutput')
        }

        mata: cols=strofreal(cols(`mataoutput'))
        mata: rows=strofreal(rows(`mataoutput'))
        mata: st_local("cols",cols[1,1])
        mata: st_local("rows",rows[1,1])


        capture file open `file' using `"`using'"' , w  text `replaceappend'
        force_override `c(rc)' 1 , cmd(file open `file' using `"`using'"' , w  text `replaceappend' ) sleep(`sleep')

        if "`preheader'" != "" {
                capture file write `file' `"`preheader' "' _n
                force_override `c(rc)' 1 , cmd(file write `file' `"`preheader'"' _n) sleep(`sleep')
        }

        forvalues i = 1(1)`rows' {
                local line
                forvalues j=1(1)`cols' {
                        mata: st_local("lineij",`mataoutput'[`i',`j'])
                        local line `line' `lineij'
                        if `j' == `cols' & `i' == 1 {
                                local line `line' `postheader'
                        }
                }
                capture file write `file' `"`line'"' _n
                force_override `c(rc)'  1 , cmd(file write `file' `"`line'"' _n) sleep(`sleep')
        }
        if "`bottom'" != "" {
                capture file write `file' `"`bottom'"' _n
                force_override `c(rc)' 1 , cmd(file write `file' `"`bottom'"' _n) sleep(`sleep')
        }

        file close `file'

        if `"`insertendrow'"' != "" {
                tempname file file1
                local S: word count `macval(insertendrow)'
                tempfile tfile
                capture file open `file' using `tfile' , w text
                force_override `c(rc)' 1 , cmd(file open `file' using `tfile' , w text ) sleep(`sleep')
                capture file open `file1' using `using' , r text
				 force_override `c(rc)' 1 , cmd(file open `file1' using `using' , r text) sleep(`sleep')
                file read `file1' temp
                while r(eof)==0{
                        capture file write `file' `"`temp'"' _n
                        force_override `c(rc)' 1 , cmd(file write `file' `"`temp'"' _n) sleep(`sleep')
                        file read `file1' temp
                }
                file close `file'
                file close `file1'

                capture file open `file' using `tfile' , r w    text
                force_override `c(rc)' 1 , cmd(file open `file' using `tfile' , r w    text) sleep(`sleep')
                capture file open `file1' using `using' , r w    text
                force_override `c(rc)' 1 , cmd(file open `file1' using `using' , r w    text ) sleep(`sleep')

                local line = 1
                file read `file' temp
                while r(eof)==0 {
                        forv s = 1(2)`S' {
                                local pointer: word `s' of `macval(insertendrow)'
                                local to:  word `=`s'+1' of `macval(insertendrow)'
                                if "`pointer'" == "`line'" {
                                        local temp `temp' `to'
                                }
                        }
                        capture file write `file1' `"`temp'"' _n
                        force_override `c(rc)' 1 , cmd(file write `file1' `"`temp'"' _n) sleep(`sleep')
                        file read `file' temp
                        local line = `line' + 1
                }
                file close `file'
                file close `file1'
        }


        local substitute `substitute' @M "`=`cols'/2'"
        if `"`macval(substitute)'"' != "" {
                tempname file file2
                local S: word count `macval(substitute)'

                tempfile tfile
                capture file open `file' using `tfile' , w text
                force_override `c(rc)' 1 , cmd(file open `file' using `tfile' , w text ) sleep(`sleep')
                capture file open `file2' using `using' , r text
                force_override `c(rc)' 1 , cmd(file open `file2' using `using' , r text ) sleep(`sleep')
                file read `file2' temp
                while r(eof)==0{
                        capture file write `file' `"`temp'"' _n
                        force_override `c(rc)' 1 , cmd(file write `file' `"`temp'"' _n) sleep(`sleep')
                        file read `file2' temp
                }
                file close `file'
                file close `file2'

                capture file open `file' using `tfile' , r    text
                force_override `c(rc)' 1 , cmd(file open `file' using `tfile' , r    text ) sleep(`sleep')

                capture file open `file2' using `using' , w    text replace
                force_override `c(rc)' 1 , cmd(file open `file2' using `using' , w    text replace) sleep(`sleep')

                file read `file' temp
                while r(eof)==0 {
                        forv s = 1(2)`S' {
                                local from: word `s' of `macval(substitute)'
                                local to:  word `=`s'+1' of `macval(substitute)'
                                if `"`macval(from)'`macval(to)'"'!="" {
                                        local temp: subinstr local temp `"`macval(from)'"' `"`macval(to)'"', all

                                }
                        }
                        capture file write `file2' `"`temp'"' _n
                        force_override `c(rc)' 1 , cmd(file write `file2' `"`temp'"' _n) sleep(`sleep')
                        file read `file' temp
                        force_override `c(rc)' 1 , cmd(file read `file' temp) sleep(`sleep')
                }
                file close `file'
                file close `file2'
        }
        di as txt `"Latex file written to {browse `using'}"'

        if "`show'" == "show" {
                capture file open `file' using `using' , r text
                force_override `c(rc)' 1 , cmd(capture file open `file' using `tfile' , r text) sleep(`sleep')
                file read `file' temp
                while r(eof) == 0 {
                        display _asis `" `macval(temp)'"'
                        file read `file' temp
                }
                file close `file'
        }
        capture mata: mata drop `mataoutput' `rownamesmatrix' `colnamesmatrix' `mfile' `mpath'
        if "`nocdset'" == ""  {
                qui cd "`cdir_old'"
        }
end

**cell by cell transformation into strings
capture mata: mata drop m_comptostr()
mata:
function m_comptostr(original_matrix , string matrix fmt)
{
		output = strofreal(J(rows(original_matrix),cols(original_matrix),.))
        i = 1
        while (i<=cols(original_matrix)) {
                temp = original_matrix[.,i]
                temp2 = strofreal(J(rows(temp),1,.))
				fmt_c = fmt[1,i]
                j = 1
                while (j<= rows(temp)) {
                        realpart = Re(temp[j,1])
                        impart = Im(temp[j,1])
                        if (impart==0) {
                                temp2[j,1] = strofreal(realpart,fmt_c)
                        }
                        if ((impart>0) & (temp[j,1]!=.)) {
                                temp2[j,1] = strofreal(realpart,fmt_c) :+ " + " :+ strofreal(impart,fmt_c) :+ "i"
                        }
                        if ((impart<0) & (temp[j,1]!=.)) {
                                temp2[j,1] = strofreal(realpart,fmt_c) :+ " - " :+ strofreal(abs(impart),fmt_c) :+ "i"
                        }
                        j++
                }
                output[.,i] = temp2
                i++
        }

        return(output)
}
end
*Converts matrix into string and format as specified by fmt
capture mata mata drop m_convert_matrix()
mata:
function m_convert_matrix(original_matrix , string scalar fmt)
{
       output = original_matrix

	   /// fmt part
		fmt = tokens(fmt)

		//fill fmt with last entry
		if (cols(fmt) < cols(original_matrix)) {
			fmt = (fmt , J(1,cols(original_matrix)-cols(fmt),fmt[1,cols(fmt)]))
		}
		if (eltype(original_matrix) == ("real") ) {
			   output = m_comptostr(output,fmt)
        }
        if (eltype(original_matrix) == ("complex") ) {
                output = m_comptostr(output,fmt)
        }
		return(output)
}
end

**adds rows and columns for delimiter
capture mata mata drop m_add_cols()
mata:
function m_add_cols(original_matrix,string scalar coldel, string scalar rowdel)
{
	mataoutput=strofreal(J(1,cols(original_matrix)*2,0))
	i = 1
	while (i<=rows(original_matrix)) {
		j = 2
		mataoutput_j = original_matrix[i,1]

		while (j<=cols(original_matrix)) {
			mataoutput_j = (mataoutput_j, coldel , original_matrix[i,j] )
			j++
		}
		mataoutput = (mataoutput \ (mataoutput_j , rowdel) )
		i++
	}
	mataoutput = mataoutput[2..rows(mataoutput),.]
	return(mataoutput)
}
end
**This program pauses the program for 500ms if a read/write command aborted with an error and repeates the command at most 5 times.
**Necessary as STATA is sometimes "too fast" for the operating system.
capture program drop force_override
program define force_override
        syntax anything , cmd(string asis) [sleep(real 500)]

        tokenize `anything'
        local rc `1'
        local run `2'

        if `run' < 5 {
                if `rc' == 603 {
                        sleep `sleep`
                        capture `cmd'
                        if `rc' != 0 {
                                force_override `c(rc)' `=`run'+1' , cmd(`cmd') sleep(`sleep')
                        }
                }

        }
        else {
                display "Accessing file 5 times failed!"
                error `rc'

        }
end

*********************************************************************************************
*********************************m_significance**********************************************
*********************************************************************************************
/*
Performs a two-sided t-test and creates a string matrix whith significance stars and
parenthesis around se if requested.

The coefficient matrix has to look like:

b(1,1)	b(1,2)
se(1,1)	se(1,2)
b(2,1)	b(2,2)
se(2,1)	se(2,2)

syntax:
	m_significance(coefficient matrix, levels, fmt,| degrees of freedom, parenthesis around SE)

	levels: can be any number of levels in the form of 0.01 0.05 etc.
	degrees of freedom: dof for t-test
	parenthesis: if selected (eg. by a 1), then parenthesis are added
*/
capture mata mata drop m_significance()
version 10
mata:
	function m_significance( coeff , real matrix level ,| real scalar df , real scalar par , real scalar addstar)
	{
		if (args() == 2) {
			df = 999
		}
		if (args() < 4) {
			par = 0
		}
		if (args() < 5) {
			addstar == 1
		}

		output = coeff
		level = 1 :- level
		j = 1
		if (addstar == 1) {
			while (j <= cols(coeff)) {
				i = 1
				while (i <= rows(coeff)) {
					b = strtoreal(coeff[i,j])
					se = strtoreal(coeff[i+1,j])
					t= t(df,abs(b:/se))
					//t= t(df,abs(coeff[i,j] :/coeff[i+1,j]))
					lev = 1
					while (lev <= cols(level)) {
						if (t > level[lev]) {
							output[i,j] = sprintf("%s*",output[i,j])
						}
						lev++
					}
					i = i + 2
				}
				j++
			}
		}
		// add brackets to 2nd row of each block
		if (par == 1) {
			if (par[1,1] == 1 ) {
				j = 1
				while (j <= cols(coeff)) {
					i = 1
					while (i <= rows(coeff)) {
						output[i+1,j] = sprintf("(%s)",output[i+1,j])

						i = i + 2
					}
					j++
				}

			}

		}
		return(output)
	}
end

***parser for coefficient options
capture program drop mmat2tex_coefficient
program define mmat2tex_coefficient, rclass
	syntax anything [, dof(string) par pos(string asis) levels(string asis) NOSTAR]

	if "`dof'" == "" {
		local dof = 999
	}
	if "`par'" == "" {
		local par = 0
	}
	else {
		local par = 1
	}
	if "`pos'" == "" {
		local ux = 1
		local uy = 1
		mata st_local("lx",strofreal(rows(`anything')))
		mata st_local("ly",strofreal(cols(`anything')))
	}
	else {
		gettoken one two : pos , match(paren)
		gettoken ux uy : one ,  parse(",")
		gettoken two : two , match(paren)
		gettoken lx ly : two , parse(",")
		local lx = subinstr("`lx'",",","",.)
		local ux = subinstr("`ux'",",","",.)
		local ly = subinstr("`ly'",",","",.)
		local uy = subinstr("`uy'",",","",.)
	}
	if "`levels'" == "" {
		local levels "0.1,0.05,0.01"
	}
	if "`nostar'" == "" {
		local star = 1
	}
	else {
		local star = 0
	}


	return local dof  =`dof'
	return local par = `par'
	return local ux = `ux'
	return local uy = `uy'
	return local lx = `lx'
	return local ly = `ly'
	return local levels "`levels'"
	return local star = `star'
end
