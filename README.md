
  <div tabindex="-1" id="notebook" class="border-box-sizing">
    <div class="container" id="notebook-container">

<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<hr>
<p><img src=(https://i.ibb.co/0f57Bb0/moneyball-4-1190x700.png"></p>
<hr>
<h1 id="MoneyBall-Project---Solutions">MoneyBall Project - Solutions<a class="anchor-link" href="#MoneyBall-Project---Solutions">¶</a></h1>
![MoneyBall](https://i.ibb.co/0f57Bb0/moneyball-4-1190x700.png)
</div>
</div>
</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<h2 id="Rules-of-Baseball">Rules of Baseball<a class="anchor-link" href="#Rules-of-Baseball">¶</a></h2><p>You don't need to know much about Baseball to complete this exercise. If you're totally unfamiliar with Baseball, check out this <a href="https://www.youtube.com/watch?v=0bKkGeROiPA">useful explanatory video!</a></p>
<hr>
<h2 id="Background"><a href="https://www.youtube.com/watch?v=yGf6LNWY9AI">Background</a><a class="anchor-link" href="#Background">¶</a></h2><p><em>Source: Wikipedia</em></p>
<h3 id="The-2002-Oakland-A's">The 2002 Oakland A's<a class="anchor-link" href="#The-2002-Oakland-A's">¶</a></h3><p>The Oakland Athletics' 2002 season was the team's 35th in Oakland, California. It was also the 102nd season in franchise history. The Athletics finished first in the American League West with a record of 103-59.</p>
<p>The Athletics' 2002 campaign ranks among the most famous in franchise history. Following the 2001 season, Oakland saw the departure of three key players (the lost boys). Billy Beane, the team's general manager, responded with a series of under-the-radar free agent signings. The new-look Athletics, despite a comparative lack of star power, surprised the baseball world by besting the 2001 team's regular season record. The team is most famous, however, for winning 20 consecutive games between August 13 and September 4, 2002.[1] The Athletics' season was the subject of Michael Lewis' 2003 book Moneyball: The Art of Winning an Unfair Game (as Lewis was given the opportunity to follow the team around throughout that season)</p>
<p>This project is based off the book written by Michael Lewis (later turned into a movie).</p>
<h3 id="Moneyball-Book">Moneyball Book<a class="anchor-link" href="#Moneyball-Book">¶</a></h3><p>The central premise of book <em>Moneyball</em> is that the collective wisdom of baseball insiders (including players, managers, coaches, scouts, and the front office) over the past century is subjective and often flawed. Statistics such as stolen bases, runs batted in, and batting average, typically used to gauge players, are relics of a 19th-century view of the game and the statistics available at that time. The book argues that the Oakland A's' front office took advantage of more analytical gauges of player performance to field a team that could better compete against richer competitors in Major League Baseball (MLB).</p>
<p>Rigorous statistical analysis had demonstrated that on-base percentage and slugging percentage are better indicators of offensive success, and the A's became convinced that these qualities were cheaper to obtain on the open market than more historically valued qualities such as speed and contact. These observations often flew in the face of conventional baseball wisdom and the beliefs of many baseball scouts and executives.</p>
<p>By re-evaluating the strategies that produce wins on the field, the 2002 Athletics, with approximately US 44 million dollars in salary, were competitive with larger market teams such as the New York Yankees, who spent over US$125 million in payroll that same season.</p>
<p><img src="salary.png" style="width: 478px; height: 315px;"></p>
<p>Because of the team's smaller revenues, Oakland is forced to find players undervalued by the market, and their system for finding value in undervalued players has proven itself thus far. This approach brought the A's to the playoffs in 2002 and 2003.</p>
<p>In this project we'll work with some data and with the goal of trying to find replacement players for the ones lost at the start of the off-season - During the 2001–02 offseason, the team lost three key free agents to larger market teams: 2000 AL MVP Jason Giambi to the New York Yankees, outfielder Johnny Damon to the Boston Red Sox, and closer Jason Isringhausen to the St. Louis Cardinals.</p>
<p>The main goal of this project is for you to feel comfortable working with R on real data to try and derive actionable insights!</p>

</div>
</div>
</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<h1 id="Let's-get-started!">Let's get started!<a class="anchor-link" href="#Let's-get-started!">¶</a></h1><p><strong> Follow the steps outlined in bold below using your new R skills and help the Oakland A's recruit under-valued players! </strong>
<img src="Moneyball.jpg"></p>

</div>
</div>
</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<h2 id="Data">Data<a class="anchor-link" href="#Data">¶</a></h2><p>We'll be using data from <a href="http://www.seanlahman.com/baseball-archive/statistics/">Sean Lahaman's Website</a> a very useful source for baseball statistics. The documentation for the csv files is located in the <strong>readme2013.txt</strong> file. You may need to reference this to understand what acronyms stand for.</p>
<p><strong> Use R to open the Batting.csv file and assign it to a dataframe called batting using <em>read.csv</em> </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[1]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>batting <span class="o">&lt;-</span> read.csv<span class="p">(</span><span class="s">'Batting.csv'</span><span class="p">)</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Use head() to check out the batting </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[2]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="kp">head</span><span class="p">(</span>batting<span class="p">)</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[2]:</div>

<div class="output_html rendered_html output_subarea output_execute_result">
<table>
<thead><tr><th></th><th scope="col">playerID</th><th scope="col">yearID</th><th scope="col">stint</th><th scope="col">teamID</th><th scope="col">lgID</th><th scope="col">G</th><th scope="col">G_batting</th><th scope="col">AB</th><th scope="col">R</th><th scope="col">H</th><th scope="col">X2B</th><th scope="col">X3B</th><th scope="col">HR</th><th scope="col">RBI</th><th scope="col">SB</th><th scope="col">CS</th><th scope="col">BB</th><th scope="col">SO</th><th scope="col">IBB</th><th scope="col">HBP</th><th scope="col">SH</th><th scope="col">SF</th><th scope="col">GIDP</th><th scope="col">G_old</th></tr></thead>
<tbody>
	<tr><th scope="row">1</th><td>aardsda01</td><td>2004</td><td>1</td><td>SFN</td><td>NL</td><td>11</td><td>11</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>11</td></tr>
	<tr><th scope="row">2</th><td>aardsda01</td><td>2006</td><td>1</td><td>CHN</td><td>NL</td><td>45</td><td>43</td><td>2</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>1</td><td>0</td><td>0</td><td>45</td></tr>
	<tr><th scope="row">3</th><td>aardsda01</td><td>2007</td><td>1</td><td>CHA</td><td>AL</td><td>25</td><td>2</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>2</td></tr>
	<tr><th scope="row">4</th><td>aardsda01</td><td>2008</td><td>1</td><td>BOS</td><td>AL</td><td>47</td><td>5</td><td>1</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>1</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>5</td></tr>
	<tr><th scope="row">5</th><td>aardsda01</td><td>2009</td><td>1</td><td>SEA</td><td>AL</td><td>73</td><td>3</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>NA</td></tr>
	<tr><th scope="row">6</th><td>aardsda01</td><td>2010</td><td>1</td><td>SEA</td><td>AL</td><td>53</td><td>4</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>NA</td></tr>
</tbody>
</table>

</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Use str() to check the structure. Pay close attention to how columns that start with a number get an 'X' in front of them! You'll need to know this to call those columns! </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[3]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>str<span class="p">(</span>batting<span class="p">)</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt"></div>
<div class="output_subarea output_stream output_stdout output_text">
<pre>'data.frame':	97889 obs. of  24 variables:
 $ playerID : Factor w/ 18107 levels "aardsda01","aaronha01",..: 1 1 1 1 1 1 1 2 2 2 ...
 $ yearID   : int  2004 2006 2007 2008 2009 2010 2012 1954 1955 1956 ...
 $ stint    : int  1 1 1 1 1 1 1 1 1 1 ...
 $ teamID   : Factor w/ 149 levels "ALT","ANA","ARI",..: 117 35 33 16 116 116 93 80 80 80 ...
 $ lgID     : Factor w/ 6 levels "AA","AL","FL",..: 4 4 2 2 2 2 2 4 4 4 ...
 $ G        : int  11 45 25 47 73 53 1 122 153 153 ...
 $ G_batting: int  11 43 2 5 3 4 NA 122 153 153 ...
 $ AB       : int  0 2 0 1 0 0 NA 468 602 609 ...
 $ R        : int  0 0 0 0 0 0 NA 58 105 106 ...
 $ H        : int  0 0 0 0 0 0 NA 131 189 200 ...
 $ X2B      : int  0 0 0 0 0 0 NA 27 37 34 ...
 $ X3B      : int  0 0 0 0 0 0 NA 6 9 14 ...
 $ HR       : int  0 0 0 0 0 0 NA 13 27 26 ...
 $ RBI      : int  0 0 0 0 0 0 NA 69 106 92 ...
 $ SB       : int  0 0 0 0 0 0 NA 2 3 2 ...
 $ CS       : int  0 0 0 0 0 0 NA 2 1 4 ...
 $ BB       : int  0 0 0 0 0 0 NA 28 49 37 ...
 $ SO       : int  0 0 0 1 0 0 NA 39 61 54 ...
 $ IBB      : int  0 0 0 0 0 0 NA NA 5 6 ...
 $ HBP      : int  0 0 0 0 0 0 NA 3 3 2 ...
 $ SH       : int  0 1 0 0 0 0 NA 6 7 5 ...
 $ SF       : int  0 0 0 0 0 0 NA 4 4 7 ...
 $ GIDP     : int  0 0 0 0 0 0 NA 13 20 21 ...
 $ G_old    : int  11 45 2 5 NA NA NA 122 153 153 ...
</pre>
</div>
</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Make sure you understand how to call the columns by using the $ symbol. </strong></p>
<p><strong> Call the head() of the first five rows of AB (At Bats) column </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[4]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="kp">head</span><span class="p">(</span>batting<span class="o">$</span>AB<span class="p">)</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[4]:</div>

<div class="output_html rendered_html output_subarea output_execute_result">
<ol class="list-inline">
	<li>0</li>
	<li>2</li>
	<li>0</li>
	<li>1</li>
	<li>0</li>
	<li>0</li>
</ol>

</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Call the head of the doubles (X2B) column </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[5]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="kp">head</span><span class="p">(</span>batting<span class="o">$</span>X2B<span class="p">)</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[5]:</div>

<div class="output_html rendered_html output_subarea output_execute_result">
<ol class="list-inline">
	<li>0</li>
	<li>0</li>
	<li>0</li>
	<li>0</li>
	<li>0</li>
	<li>0</li>
</ol>

</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Quick Note: If you used fread() to use data.table, then you won't need to worry about these X in front of numbers, instead you would use something like: </strong></p>

<pre><code>batting[,'2B',with=FALSE]
</code></pre>
<p>There's a few more ways of doing detailed <a href="http://stackoverflow.com/questions/15637132/how-to-reference-column-names-that-start-with-a-number-in-data-table">here</a>.</p>
<p><strong>Alright! Let's move on! </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<h3 id="Feature-Engineering">Feature Engineering<a class="anchor-link" href="#Feature-Engineering">¶</a></h3><p>We need to add three more statistics that were used in Moneyball! These are:</p>
<ul>
<li><a href="https://en.wikipedia.org/wiki/Batting_average">Batting Average</a></li>
<li><a href="http://en.wikipedia.org/wiki/On-base_percentage">On Base Percentage</a></li>
<li><a href="http://en.wikipedia.org/wiki/Slugging_percentage">Slugging Percentage</a></li>
</ul>
<p>Click on the links provided and search the wikipedia page for the formula for creating the new statistic! For example, for Batting Average, you'll need to scroll down until you see:</p>
$$ AVG = \frac{H}{AB} $$<p>Which means that the Batting Average is equal to H (Hits) divided by AB (At Base). So we'll do the following to create a new column called <strong>BA</strong> and add it to our data frame:</p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[6]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>batting<span class="o">$</span>BA <span class="o">&lt;-</span> batting<span class="o">$</span>H <span class="o">/</span> batting<span class="o">$</span>AB
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> After doing this operation, check the last 5 entries of the BA column of your data frame and it should look like this: </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[7]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="kp">tail</span><span class="p">(</span>batting<span class="o">$</span>BA<span class="p">,</span><span class="m">5</span><span class="p">)</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[7]:</div>

<div class="output_html rendered_html output_subarea output_execute_result">
<ol class="list-inline">
	<li>0.123076923076923</li>
	<li>0.274647887323944</li>
	<li>0.147058823529412</li>
	<li>0.274509803921569</li>
	<li>0.213872832369942</li>
</ol>

</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Now do the same for some new columns! On Base Percentage (OBP) and Slugging Percentage (SLG). Hint: For SLG, you need 1B (Singles), this isn't in your data frame. However you can calculate it by subtracting doubles,triples, and home runs from total hits (H):</strong>    <strong>1B = H-2B-3B-HR </strong></p>
<ul>
<li><strong>Create an OBP Column </strong></li>
<li><strong> Create an SLG Column </strong></li>
</ul>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[8]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="c1"># On Base Percentage</span>
batting<span class="o">$</span>OBP <span class="o">&lt;-</span> <span class="p">(</span>batting<span class="o">$</span>H <span class="o">+</span> batting<span class="o">$</span>BB <span class="o">+</span> batting<span class="o">$</span>HBP<span class="p">)</span><span class="o">/</span><span class="p">(</span>batting<span class="o">$</span>AB <span class="o">+</span> batting<span class="o">$</span>BB <span class="o">+</span> batting<span class="o">$</span>HBP <span class="o">+</span> batting<span class="o">$</span>SF<span class="p">)</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[9]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="c1"># Creating X1B (Singles)</span>
batting<span class="o">$</span>X1B <span class="o">&lt;-</span> batting<span class="o">$</span>H <span class="o">-</span> batting<span class="o">$</span>X2B <span class="o">-</span> batting<span class="o">$</span>X3B <span class="o">-</span> batting<span class="o">$</span>HR
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[10]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="c1"># Creating Slugging Average (SLG)</span>
batting<span class="o">$</span>SLG <span class="o">&lt;-</span> <span class="p">((</span><span class="m">1</span> <span class="o">*</span> batting<span class="o">$</span>X1B<span class="p">)</span> <span class="o">+</span> <span class="p">(</span><span class="m">2</span> <span class="o">*</span> batting<span class="o">$</span>X2B<span class="p">)</span> <span class="o">+</span> <span class="p">(</span><span class="m">3</span> <span class="o">*</span> batting<span class="o">$</span>X3B<span class="p">)</span> <span class="o">+</span> <span class="p">(</span><span class="m">4</span> <span class="o">*</span> batting<span class="o">$</span>HR<span class="p">)</span> <span class="p">)</span> <span class="o">/</span> batting<span class="o">$</span>AB
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Check the structure of your data frame using str() </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[11]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>str<span class="p">(</span>batting<span class="p">)</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt"></div>
<div class="output_subarea output_stream output_stdout output_text">
<pre>'data.frame':	97889 obs. of  28 variables:
 $ playerID : Factor w/ 18107 levels "aardsda01","aaronha01",..: 1 1 1 1 1 1 1 2 2 2 ...
 $ yearID   : int  2004 2006 2007 2008 2009 2010 2012 1954 1955 1956 ...
 $ stint    : int  1 1 1 1 1 1 1 1 1 1 ...
 $ teamID   : Factor w/ 149 levels "ALT","ANA","ARI",..: 117 35 33 16 116 116 93 80 80 80 ...
 $ lgID     : Factor w/ 6 levels "AA","AL","FL",..: 4 4 2 2 2 2 2 4 4 4 ...
 $ G        : int  11 45 25 47 73 53 1 122 153 153 ...
 $ G_batting: int  11 43 2 5 3 4 NA 122 153 153 ...
 $ AB       : int  0 2 0 1 0 0 NA 468 602 609 ...
 $ R        : int  0 0 0 0 0 0 NA 58 105 106 ...
 $ H        : int  0 0 0 0 0 0 NA 131 189 200 ...
 $ X2B      : int  0 0 0 0 0 0 NA 27 37 34 ...
 $ X3B      : int  0 0 0 0 0 0 NA 6 9 14 ...
 $ HR       : int  0 0 0 0 0 0 NA 13 27 26 ...
 $ RBI      : int  0 0 0 0 0 0 NA 69 106 92 ...
 $ SB       : int  0 0 0 0 0 0 NA 2 3 2 ...
 $ CS       : int  0 0 0 0 0 0 NA 2 1 4 ...
 $ BB       : int  0 0 0 0 0 0 NA 28 49 37 ...
 $ SO       : int  0 0 0 1 0 0 NA 39 61 54 ...
 $ IBB      : int  0 0 0 0 0 0 NA NA 5 6 ...
 $ HBP      : int  0 0 0 0 0 0 NA 3 3 2 ...
 $ SH       : int  0 1 0 0 0 0 NA 6 7 5 ...
 $ SF       : int  0 0 0 0 0 0 NA 4 4 7 ...
 $ GIDP     : int  0 0 0 0 0 0 NA 13 20 21 ...
 $ G_old    : int  11 45 2 5 NA NA NA 122 153 153 ...
 $ BA       : num  NaN 0 NaN 0 NaN ...
 $ OBP      : num  NaN 0 NaN 0 NaN ...
 $ X1B      : int  0 0 0 0 0 0 NA 85 116 126 ...
 $ SLG      : num  NaN 0 NaN 0 NaN ...
</pre>
</div>
</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<hr>
<h2 id="Merging-Salary-Data-with-Batting-Data">Merging Salary Data with Batting Data<a class="anchor-link" href="#Merging-Salary-Data-with-Batting-Data">¶</a></h2><p>We know we don't just want the best players, we want the most <em>undervalued</em> players, meaning we will also need to know current salary information! We have salary information in the csv file 'Salaries.csv'.</p>
<p>Complete the following steps to merge the salary data with the player stats!</p>

</div>
</div>
</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Load the Salaries.csv file into a dataframe called sal using read.csv </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[12]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>sal <span class="o">&lt;-</span> read.csv<span class="p">(</span><span class="s">'Salaries.csv'</span><span class="p">)</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Use summary to get a summary of the batting data frame and notice the minimum year in the yearID column. Our batting data goes back to 1871! Our salary data starts at 1985, meaning we need to remove the batting data that occured before 1985.</strong></p>
<p><strong> Use subset() to reassign batting to only contain data from 1985 and onwards </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[13]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="kp">summary</span><span class="p">(</span>batting<span class="p">)</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[13]:</div>


<div class="output_text output_subarea output_execute_result">
<pre>      playerID         yearID         stint           teamID        lgID      
 mcguide01:   31   Min.   :1871   Min.   :1.000   CHN    : 4720   AA  : 1890  
 henderi01:   29   1st Qu.:1931   1st Qu.:1.000   PHI    : 4621   AL  :44369  
 newsobo01:   29   Median :1970   Median :1.000   PIT    : 4575   FL  :  470  
 johnto01 :   28   Mean   :1962   Mean   :1.077   SLN    : 4535   NL  :49944  
 kaatji01 :   28   3rd Qu.:1995   3rd Qu.:1.000   CIN    : 4393   PL  :  147  
 ansonca01:   27   Max.   :2013   Max.   :5.000   CLE    : 4318   UA  :  332  
 (Other)  :97717                                  (Other):70727   NA's:  737  
       G            G_batting            AB              R         
 Min.   :  1.00   Min.   :  0.00   Min.   :  0.0   Min.   :  0.00  
 1st Qu.: 13.00   1st Qu.:  7.00   1st Qu.:  9.0   1st Qu.:  0.00  
 Median : 35.00   Median : 32.00   Median : 61.0   Median :  5.00  
 Mean   : 51.65   Mean   : 49.13   Mean   :154.1   Mean   : 20.47  
 3rd Qu.: 81.00   3rd Qu.: 81.00   3rd Qu.:260.0   3rd Qu.: 31.00  
 Max.   :165.00   Max.   :165.00   Max.   :716.0   Max.   :192.00  
                  NA's   :1406     NA's   :6413    NA's   :6413    
       H               X2B            X3B               HR        
 Min.   :  0.00   Min.   : 0.0   Min.   : 0.000   Min.   : 0.000  
 1st Qu.:  1.00   1st Qu.: 0.0   1st Qu.: 0.000   1st Qu.: 0.000  
 Median : 12.00   Median : 2.0   Median : 0.000   Median : 0.000  
 Mean   : 40.37   Mean   : 6.8   Mean   : 1.424   Mean   : 3.002  
 3rd Qu.: 66.00   3rd Qu.:10.0   3rd Qu.: 2.000   3rd Qu.: 3.000  
 Max.   :262.00   Max.   :67.0   Max.   :36.000   Max.   :73.000  
 NA's   :6413     NA's   :6413   NA's   :6413     NA's   :6413    
      RBI               SB                CS               BB        
 Min.   :  0.00   Min.   :  0.000   Min.   : 0.000   Min.   :  0.00  
 1st Qu.:  0.00   1st Qu.:  0.000   1st Qu.: 0.000   1st Qu.:  0.00  
 Median :  5.00   Median :  0.000   Median : 0.000   Median :  4.00  
 Mean   : 18.47   Mean   :  3.265   Mean   : 1.385   Mean   : 14.21  
 3rd Qu.: 28.00   3rd Qu.:  2.000   3rd Qu.: 1.000   3rd Qu.: 21.00  
 Max.   :191.00   Max.   :138.000   Max.   :42.000   Max.   :232.00  
 NA's   :6837     NA's   :7713      NA's   :29867    NA's   :6413    
       SO              IBB              HBP               SH        
 Min.   :  0.00   Min.   :  0.00   Min.   : 0.000   Min.   : 0.000  
 1st Qu.:  2.00   1st Qu.:  0.00   1st Qu.: 0.000   1st Qu.: 0.000  
 Median : 11.00   Median :  0.00   Median : 0.000   Median : 1.000  
 Mean   : 21.95   Mean   :  1.28   Mean   : 1.136   Mean   : 2.564  
 3rd Qu.: 31.00   3rd Qu.:  1.00   3rd Qu.: 1.000   3rd Qu.: 3.000  
 Max.   :223.00   Max.   :120.00   Max.   :51.000   Max.   :67.000  
 NA's   :14251    NA's   :42977    NA's   :9233     NA's   :12751   
       SF             GIDP           G_old              BA       
 Min.   : 0.0    Min.   : 0.00   Min.   :  0.00   Min.   :0.000  
 1st Qu.: 0.0    1st Qu.: 0.00   1st Qu.: 11.00   1st Qu.:0.148  
 Median : 0.0    Median : 1.00   Median : 34.00   Median :0.231  
 Mean   : 1.2    Mean   : 3.33   Mean   : 50.99   Mean   :0.209  
 3rd Qu.: 2.0    3rd Qu.: 5.00   3rd Qu.: 82.00   3rd Qu.:0.275  
 Max.   :19.0    Max.   :36.00   Max.   :165.00   Max.   :1.000  
 NA's   :42446   NA's   :32521   NA's   :5189     NA's   :13520  
      OBP             X1B              SLG       
 Min.   :0.00    Min.   :  0.00   Min.   :0.000  
 1st Qu.:0.19    1st Qu.:  1.00   1st Qu.:0.179  
 Median :0.29    Median :  9.00   Median :0.309  
 Mean   :0.26    Mean   : 29.14   Mean   :0.291  
 3rd Qu.:0.34    3rd Qu.: 48.00   3rd Qu.:0.397  
 Max.   :1.00    Max.   :225.00   Max.   :4.000  
 NA's   :49115   NA's   :6413     NA's   :13520  </pre>
</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[14]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>batting <span class="o">&lt;-</span> <span class="kp">subset</span><span class="p">(</span>batting<span class="p">,</span>yearID <span class="o">&gt;=</span> <span class="m">1985</span><span class="p">)</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Now use summary again to make sure the subset reassignment worked, your yearID min should be 1985 </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[15]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="kp">summary</span><span class="p">(</span>batting<span class="p">)</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[15]:</div>


<div class="output_text output_subarea output_execute_result">
<pre>      playerID         yearID         stint          teamID      lgID      
 moyerja01:   27   Min.   :1985   Min.   :1.00   SDN    : 1313   AA:    0  
 mulhote01:   26   1st Qu.:1993   1st Qu.:1.00   CLE    : 1306   AL:17226  
 weathda01:   26   Median :2000   Median :1.00   PIT    : 1299   FL:    0  
 maddugr01:   25   Mean   :2000   Mean   :1.08   NYN    : 1297   NL:18426  
 sierrru01:   25   3rd Qu.:2007   3rd Qu.:1.00   BOS    : 1279   PL:    0  
 thomeji01:   25   Max.   :2013   Max.   :4.00   CIN    : 1279   UA:    0  
 (Other)  :35498                                 (Other):27879             
       G           G_batting            AB              R         
 Min.   :  1.0   Min.   :  0.00   Min.   :  0.0   Min.   :  0.00  
 1st Qu.: 14.0   1st Qu.:  4.00   1st Qu.:  3.0   1st Qu.:  0.00  
 Median : 34.0   Median : 27.00   Median : 47.0   Median :  4.00  
 Mean   : 51.7   Mean   : 46.28   Mean   :144.7   Mean   : 19.44  
 3rd Qu.: 77.0   3rd Qu.: 77.00   3rd Qu.:241.0   3rd Qu.: 30.00  
 Max.   :163.0   Max.   :163.00   Max.   :716.0   Max.   :152.00  
                 NA's   :1406     NA's   :4377    NA's   :4377    
       H               X2B              X3B               HR        
 Min.   :  0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
 1st Qu.:  0.00   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
 Median :  8.00   Median : 1.000   Median : 0.000   Median : 0.000  
 Mean   : 37.95   Mean   : 7.293   Mean   : 0.824   Mean   : 4.169  
 3rd Qu.: 61.00   3rd Qu.:11.000   3rd Qu.: 1.000   3rd Qu.: 5.000  
 Max.   :262.00   Max.   :59.000   Max.   :23.000   Max.   :73.000  
 NA's   :4377     NA's   :4377     NA's   :4377     NA's   :4377    
      RBI               SB                CS               BB        
 Min.   :  0.00   Min.   :  0.000   Min.   : 0.000   Min.   :  0.00  
 1st Qu.:  0.00   1st Qu.:  0.000   1st Qu.: 0.000   1st Qu.:  0.00  
 Median :  3.00   Median :  0.000   Median : 0.000   Median :  3.00  
 Mean   : 18.41   Mean   :  2.811   Mean   : 1.219   Mean   : 14.06  
 3rd Qu.: 27.00   3rd Qu.:  2.000   3rd Qu.: 1.000   3rd Qu.: 21.00  
 Max.   :165.00   Max.   :110.000   Max.   :29.000   Max.   :232.00  
 NA's   :4377     NA's   :4377      NA's   :4377     NA's   :4377    
       SO              IBB               HBP               SH        
 Min.   :  0.00   Min.   :  0.000   Min.   : 0.000   Min.   : 0.000  
 1st Qu.:  1.00   1st Qu.:  0.000   1st Qu.: 0.000   1st Qu.: 0.000  
 Median : 12.00   Median :  0.000   Median : 0.000   Median : 0.000  
 Mean   : 27.03   Mean   :  1.171   Mean   : 1.273   Mean   : 1.465  
 3rd Qu.: 42.00   3rd Qu.:  1.000   3rd Qu.: 1.000   3rd Qu.: 2.000  
 Max.   :223.00   Max.   :120.000   Max.   :35.000   Max.   :39.000  
 NA's   :4377     NA's   :4378      NA's   :4387     NA's   :4377    
       SF              GIDP           G_old             BA       
 Min.   : 0.000   Min.   : 0.00   Min.   :  0.0   Min.   :0.000  
 1st Qu.: 0.000   1st Qu.: 0.00   1st Qu.: 11.0   1st Qu.:0.136  
 Median : 0.000   Median : 1.00   Median : 32.0   Median :0.233  
 Mean   : 1.212   Mean   : 3.25   Mean   : 49.7   Mean   :0.205  
 3rd Qu.: 2.000   3rd Qu.: 5.00   3rd Qu.: 77.0   3rd Qu.:0.274  
 Max.   :17.000   Max.   :35.00   Max.   :163.0   Max.   :1.000  
 NA's   :4378     NA's   :4377    NA's   :5189    NA's   :8905   
      OBP             X1B              SLG       
 Min.   :0.000   Min.   :  0.00   Min.   :0.000  
 1st Qu.:0.188   1st Qu.:  0.00   1st Qu.:0.167  
 Median :0.296   Median :  6.00   Median :0.333  
 Mean   :0.262   Mean   : 25.66   Mean   :0.304  
 3rd Qu.:0.342   3rd Qu.: 42.00   3rd Qu.:0.423  
 Max.   :1.000   Max.   :225.00   Max.   :4.000  
 NA's   :8821    NA's   :4377     NA's   :8905   </pre>
</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p>Now it is time to merge the batting data with the salary data! Since we have players playing multiple years, we'll have repetitions of playerIDs for multiple years, meaning we want to merge on <em>both</em> players and years.</p>
<p><strong> Use the merge() function to merge the batting and sal data frames by c('playerID','yearID'). Call the new data frame combo</strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[16]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>combo <span class="o">&lt;-</span> <span class="kp">merge</span><span class="p">(</span>batting<span class="p">,</span>sal<span class="p">,</span>by<span class="o">=</span><span class="kt">c</span><span class="p">(</span><span class="s">'playerID'</span><span class="p">,</span><span class="s">'yearID'</span><span class="p">))</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Use summary to check the data </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[17]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="kp">summary</span><span class="p">(</span>combo<span class="p">)</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[17]:</div>


<div class="output_text output_subarea output_execute_result">
<pre>      playerID         yearID         stint          teamID.x     lgID.x    
 moyerja01:   27   Min.   :1985   Min.   :1.000   LAN    :  940   AA:    0  
 thomeji01:   25   1st Qu.:1993   1st Qu.:1.000   PHI    :  937   AL:12292  
 weathda01:   25   Median :1999   Median :1.000   BOS    :  935   FL:    0  
 vizquom01:   24   Mean   :1999   Mean   :1.098   NYA    :  928   NL:13105  
 gaettga01:   23   3rd Qu.:2006   3rd Qu.:1.000   CLE    :  920   PL:    0  
 griffke02:   23   Max.   :2013   Max.   :4.000   SDN    :  914   UA:    0  
 (Other)  :25250                                  (Other):19823             
       G            G_batting            AB              R         
 Min.   :  1.00   Min.   :  0.00   Min.   :  0.0   Min.   :  0.00  
 1st Qu.: 26.00   1st Qu.:  8.00   1st Qu.:  5.0   1st Qu.:  0.00  
 Median : 50.00   Median : 42.00   Median : 85.0   Median :  9.00  
 Mean   : 64.06   Mean   : 57.58   Mean   :182.4   Mean   : 24.71  
 3rd Qu.:101.00   3rd Qu.:101.00   3rd Qu.:336.0   3rd Qu.: 43.00  
 Max.   :163.00   Max.   :163.00   Max.   :716.0   Max.   :152.00  
                  NA's   :906      NA's   :2661    NA's   :2661    
       H               X2B              X3B               HR        
 Min.   :  0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
 1st Qu.:  1.00   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
 Median : 19.00   Median : 3.000   Median : 0.000   Median : 1.000  
 Mean   : 48.18   Mean   : 9.276   Mean   : 1.033   Mean   : 5.369  
 3rd Qu.: 87.25   3rd Qu.:16.000   3rd Qu.: 1.000   3rd Qu.: 7.000  
 Max.   :262.00   Max.   :59.000   Max.   :23.000   Max.   :73.000  
 NA's   :2661     NA's   :2661     NA's   :2661     NA's   :2661    
      RBI               SB                CS              BB        
 Min.   :  0.00   Min.   :  0.000   Min.   : 0.00   Min.   :  0.00  
 1st Qu.:  0.00   1st Qu.:  0.000   1st Qu.: 0.00   1st Qu.:  0.00  
 Median :  8.00   Median :  0.000   Median : 0.00   Median :  6.00  
 Mean   : 23.56   Mean   :  3.568   Mean   : 1.54   Mean   : 17.98  
 3rd Qu.: 39.00   3rd Qu.:  3.000   3rd Qu.: 2.00   3rd Qu.: 29.00  
 Max.   :165.00   Max.   :110.000   Max.   :29.00   Max.   :232.00  
 NA's   :2661     NA's   :2661      NA's   :2661    NA's   :2661    
       SO              IBB               HBP               SH        
 Min.   :  0.00   Min.   :  0.000   Min.   : 0.000   Min.   : 0.000  
 1st Qu.:  2.00   1st Qu.:  0.000   1st Qu.: 0.000   1st Qu.: 0.000  
 Median : 20.00   Median :  0.000   Median : 0.000   Median : 0.000  
 Mean   : 33.52   Mean   :  1.533   Mean   : 1.614   Mean   : 1.786  
 3rd Qu.: 55.00   3rd Qu.:  2.000   3rd Qu.: 2.000   3rd Qu.: 2.000  
 Max.   :223.00   Max.   :120.000   Max.   :35.000   Max.   :39.000  
 NA's   :2661     NA's   :2662      NA's   :2670     NA's   :2661    
       SF              GIDP            G_old              BA       
 Min.   : 0.000   Min.   : 0.000   Min.   :  0.00   Min.   :0.000  
 1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 20.00   1st Qu.:0.160  
 Median : 0.000   Median : 2.000   Median : 47.00   Median :0.242  
 Mean   : 1.554   Mean   : 4.127   Mean   : 61.43   Mean   :0.212  
 3rd Qu.: 2.000   3rd Qu.: 7.000   3rd Qu.:101.00   3rd Qu.:0.276  
 Max.   :17.000   Max.   :35.000   Max.   :163.00   Max.   :1.000  
 NA's   :2662     NA's   :2661     NA's   :3414     NA's   :5618   
      OBP             X1B             SLG           teamID.y     lgID.y    
 Min.   :0.000   Min.   :  0.0   Min.   :0.000   CLE    :  935   AL:12304  
 1st Qu.:0.208   1st Qu.:  0.0   1st Qu.:0.200   PIT    :  932   NL:13093  
 Median :0.305   Median : 13.0   Median :0.351   PHI    :  931             
 Mean   :0.270   Mean   : 32.5   Mean   :0.317   SDN    :  923             
 3rd Qu.:0.346   3rd Qu.: 59.0   3rd Qu.:0.432   LAN    :  921             
 Max.   :1.000   Max.   :225.0   Max.   :4.000   CIN    :  912             
 NA's   :5562    NA's   :2661    NA's   :5618    (Other):19843             
     salary        
 Min.   :       0  
 1st Qu.:  255000  
 Median :  550000  
 Mean   : 1879256  
 3rd Qu.: 2150000  
 Max.   :33000000  
                   </pre>
</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<h2 id="Analyzing-the-Lost-Players">Analyzing the Lost Players<a class="anchor-link" href="#Analyzing-the-Lost-Players">¶</a></h2><p>As previously mentioned, the Oakland A's lost 3 key players during the off-season. We'll want to get their stats to see what we have to replace.  The players lost were: first baseman 2000 AL MVP Jason Giambi (giambja01) to the New York Yankees, outfielder Johnny Damon (damonjo01) to the Boston Red Sox and infielder Rainer Gustavo "Ray" Olmedo ('saenzol01').</p>
<p><strong> Use the subset() function to get a data frame called lost_players from the combo data frame consisting of those 3 players. Hint: Try to figure out how to use %in% to avoid a bunch of or statements!</strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[18]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>lost_players <span class="o">&lt;-</span> <span class="kp">subset</span><span class="p">(</span>combo<span class="p">,</span>playerID <span class="o">%in%</span> <span class="kt">c</span><span class="p">(</span><span class="s">'giambja01'</span><span class="p">,</span><span class="s">'damonjo01'</span><span class="p">,</span><span class="s">'saenzol01'</span><span class="p">)</span> <span class="p">)</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[19]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>lost_players
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[19]:</div>

<div class="output_html rendered_html output_subarea output_execute_result">
<table>
<thead><tr><th></th><th scope="col">playerID</th><th scope="col">yearID</th><th scope="col">stint</th><th scope="col">teamID.x</th><th scope="col">lgID.x</th><th scope="col">G</th><th scope="col">G_batting</th><th scope="col">AB</th><th scope="col">R</th><th scope="col">H</th><th scope="col">X2B</th><th scope="col">X3B</th><th scope="col">HR</th><th scope="col">RBI</th><th scope="col">SB</th><th scope="col">CS</th><th scope="col">BB</th><th scope="col">SO</th><th scope="col">IBB</th><th scope="col">HBP</th><th scope="col">SH</th><th scope="col">SF</th><th scope="col">GIDP</th><th scope="col">G_old</th><th scope="col">BA</th><th scope="col">OBP</th><th scope="col">X1B</th><th scope="col">SLG</th><th scope="col">teamID.y</th><th scope="col">lgID.y</th><th scope="col">salary</th></tr></thead>
<tbody>
	<tr><th scope="row">5135</th><td>damonjo01</td><td>1995</td><td>1</td><td>KCA</td><td>AL</td><td>47</td><td>47</td><td>188</td><td>32</td><td>53</td><td>11</td><td>5</td><td>3</td><td>23</td><td>7</td><td>0</td><td>12</td><td>22</td><td>0</td><td>1</td><td>2</td><td>3</td><td>2</td><td>47</td><td>0.2819149</td><td>0.3235294</td><td>34</td><td>0.4414894</td><td>KCA</td><td>AL</td><td>109000</td></tr>
	<tr><th scope="row">5136</th><td>damonjo01</td><td>1996</td><td>1</td><td>KCA</td><td>AL</td><td>145</td><td>145</td><td>517</td><td>61</td><td>140</td><td>22</td><td>5</td><td>6</td><td>50</td><td>25</td><td>5</td><td>31</td><td>64</td><td>3</td><td>3</td><td>10</td><td>5</td><td>4</td><td>145</td><td>0.270793</td><td>0.3129496</td><td>107</td><td>0.3675048</td><td>KCA</td><td>AL</td><td>180000</td></tr>
	<tr><th scope="row">5137</th><td>damonjo01</td><td>1997</td><td>1</td><td>KCA</td><td>AL</td><td>146</td><td>146</td><td>472</td><td>70</td><td>130</td><td>12</td><td>8</td><td>8</td><td>48</td><td>16</td><td>10</td><td>42</td><td>70</td><td>2</td><td>3</td><td>6</td><td>1</td><td>3</td><td>146</td><td>0.2754237</td><td>0.3378378</td><td>102</td><td>0.3855932</td><td>KCA</td><td>AL</td><td>240000</td></tr>
	<tr><th scope="row">5138</th><td>damonjo01</td><td>1998</td><td>1</td><td>KCA</td><td>AL</td><td>161</td><td>161</td><td>642</td><td>104</td><td>178</td><td>30</td><td>10</td><td>18</td><td>66</td><td>26</td><td>12</td><td>58</td><td>84</td><td>4</td><td>4</td><td>3</td><td>3</td><td>4</td><td>161</td><td>0.2772586</td><td>0.3394625</td><td>120</td><td>0.4392523</td><td>KCA</td><td>AL</td><td>460000</td></tr>
	<tr><th scope="row">5139</th><td>damonjo01</td><td>1999</td><td>1</td><td>KCA</td><td>AL</td><td>145</td><td>145</td><td>583</td><td>101</td><td>179</td><td>39</td><td>9</td><td>14</td><td>77</td><td>36</td><td>6</td><td>67</td><td>50</td><td>5</td><td>3</td><td>3</td><td>4</td><td>13</td><td>145</td><td>0.3070326</td><td>0.3789954</td><td>117</td><td>0.4768439</td><td>KCA</td><td>AL</td><td>2100000</td></tr>
	<tr><th scope="row">5140</th><td>damonjo01</td><td>2000</td><td>1</td><td>KCA</td><td>AL</td><td>159</td><td>159</td><td>655</td><td>136</td><td>214</td><td>42</td><td>10</td><td>16</td><td>88</td><td>46</td><td>9</td><td>65</td><td>60</td><td>4</td><td>1</td><td>8</td><td>12</td><td>7</td><td>159</td><td>0.3267176</td><td>0.3819918</td><td>146</td><td>0.4946565</td><td>KCA</td><td>AL</td><td>4000000</td></tr>
	<tr><th scope="row">5141</th><td>damonjo01</td><td>2001</td><td>1</td><td>OAK</td><td>AL</td><td>155</td><td>155</td><td>644</td><td>108</td><td>165</td><td>34</td><td>4</td><td>9</td><td>49</td><td>27</td><td>12</td><td>61</td><td>70</td><td>1</td><td>5</td><td>5</td><td>4</td><td>7</td><td>155</td><td>0.2562112</td><td>0.3235294</td><td>118</td><td>0.363354</td><td>OAK</td><td>AL</td><td>7100000</td></tr>
	<tr><th scope="row">5142</th><td>damonjo01</td><td>2002</td><td>1</td><td>BOS</td><td>AL</td><td>154</td><td>154</td><td>623</td><td>118</td><td>178</td><td>34</td><td>11</td><td>14</td><td>63</td><td>31</td><td>6</td><td>65</td><td>70</td><td>5</td><td>6</td><td>3</td><td>5</td><td>4</td><td>154</td><td>0.2857143</td><td>0.3562232</td><td>119</td><td>0.4430177</td><td>BOS</td><td>AL</td><td>7250000</td></tr>
	<tr><th scope="row">5143</th><td>damonjo01</td><td>2003</td><td>1</td><td>BOS</td><td>AL</td><td>145</td><td>145</td><td>608</td><td>103</td><td>166</td><td>32</td><td>6</td><td>12</td><td>67</td><td>30</td><td>6</td><td>68</td><td>74</td><td>4</td><td>2</td><td>6</td><td>6</td><td>5</td><td>145</td><td>0.2730263</td><td>0.3450292</td><td>116</td><td>0.4046053</td><td>BOS</td><td>AL</td><td>7500000</td></tr>
	<tr><th scope="row">5144</th><td>damonjo01</td><td>2004</td><td>1</td><td>BOS</td><td>AL</td><td>150</td><td>150</td><td>621</td><td>123</td><td>189</td><td>35</td><td>6</td><td>20</td><td>94</td><td>19</td><td>8</td><td>76</td><td>71</td><td>1</td><td>2</td><td>0</td><td>3</td><td>8</td><td>150</td><td>0.3043478</td><td>0.3803419</td><td>128</td><td>0.4766506</td><td>BOS</td><td>AL</td><td>8000000</td></tr>
	<tr><th scope="row">5145</th><td>damonjo01</td><td>2005</td><td>1</td><td>BOS</td><td>AL</td><td>148</td><td>148</td><td>624</td><td>117</td><td>197</td><td>35</td><td>6</td><td>10</td><td>75</td><td>18</td><td>1</td><td>53</td><td>69</td><td>3</td><td>2</td><td>0</td><td>9</td><td>5</td><td>148</td><td>0.3157051</td><td>0.3662791</td><td>146</td><td>0.4391026</td><td>BOS</td><td>AL</td><td>8250000</td></tr>
	<tr><th scope="row">5146</th><td>damonjo01</td><td>2006</td><td>1</td><td>NYA</td><td>AL</td><td>149</td><td>149</td><td>593</td><td>115</td><td>169</td><td>35</td><td>5</td><td>24</td><td>80</td><td>25</td><td>10</td><td>67</td><td>85</td><td>1</td><td>4</td><td>2</td><td>5</td><td>4</td><td>149</td><td>0.2849916</td><td>0.3587444</td><td>105</td><td>0.4822934</td><td>NYA</td><td>AL</td><td>13000000</td></tr>
	<tr><th scope="row">5147</th><td>damonjo01</td><td>2007</td><td>1</td><td>NYA</td><td>AL</td><td>141</td><td>141</td><td>533</td><td>93</td><td>144</td><td>27</td><td>2</td><td>12</td><td>63</td><td>27</td><td>3</td><td>66</td><td>79</td><td>1</td><td>2</td><td>1</td><td>3</td><td>4</td><td>141</td><td>0.2701689</td><td>0.3509934</td><td>103</td><td>0.3958724</td><td>NYA</td><td>AL</td><td>13000000</td></tr>
	<tr><th scope="row">5148</th><td>damonjo01</td><td>2008</td><td>1</td><td>NYA</td><td>AL</td><td>143</td><td>143</td><td>555</td><td>95</td><td>168</td><td>27</td><td>5</td><td>17</td><td>71</td><td>29</td><td>8</td><td>64</td><td>82</td><td>0</td><td>1</td><td>2</td><td>1</td><td>5</td><td>143</td><td>0.3027027</td><td>0.3752013</td><td>119</td><td>0.4612613</td><td>NYA</td><td>AL</td><td>13000000</td></tr>
	<tr><th scope="row">5149</th><td>damonjo01</td><td>2009</td><td>1</td><td>NYA</td><td>AL</td><td>143</td><td>143</td><td>550</td><td>107</td><td>155</td><td>36</td><td>3</td><td>24</td><td>82</td><td>12</td><td>0</td><td>71</td><td>98</td><td>1</td><td>2</td><td>2</td><td>1</td><td>9</td><td>NA</td><td>0.2818182</td><td>0.3653846</td><td>92</td><td>0.4890909</td><td>NYA</td><td>AL</td><td>13000000</td></tr>
	<tr><th scope="row">5150</th><td>damonjo01</td><td>2010</td><td>1</td><td>DET</td><td>AL</td><td>145</td><td>145</td><td>539</td><td>81</td><td>146</td><td>36</td><td>5</td><td>8</td><td>51</td><td>11</td><td>1</td><td>69</td><td>90</td><td>2</td><td>2</td><td>2</td><td>1</td><td>5</td><td>NA</td><td>0.270872</td><td>0.3551555</td><td>97</td><td>0.4007421</td><td>DET</td><td>AL</td><td>8000000</td></tr>
	<tr><th scope="row">5151</th><td>damonjo01</td><td>2011</td><td>1</td><td>TBA</td><td>AL</td><td>150</td><td>150</td><td>582</td><td>79</td><td>152</td><td>29</td><td>7</td><td>16</td><td>73</td><td>19</td><td>6</td><td>51</td><td>92</td><td>1</td><td>7</td><td>2</td><td>5</td><td>4</td><td>150</td><td>0.2611684</td><td>0.3255814</td><td>100</td><td>0.4175258</td><td>TBA</td><td>AL</td><td>5250000</td></tr>
	<tr><th scope="row">7872</th><td>giambja01</td><td>1995</td><td>1</td><td>OAK</td><td>AL</td><td>54</td><td>54</td><td>176</td><td>27</td><td>45</td><td>7</td><td>0</td><td>6</td><td>25</td><td>2</td><td>1</td><td>28</td><td>31</td><td>0</td><td>3</td><td>1</td><td>2</td><td>4</td><td>54</td><td>0.2556818</td><td>0.3636364</td><td>32</td><td>0.3977273</td><td>OAK</td><td>AL</td><td>109000</td></tr>
	<tr><th scope="row">7873</th><td>giambja01</td><td>1996</td><td>1</td><td>OAK</td><td>AL</td><td>140</td><td>140</td><td>536</td><td>84</td><td>156</td><td>40</td><td>1</td><td>20</td><td>79</td><td>0</td><td>1</td><td>51</td><td>95</td><td>3</td><td>5</td><td>1</td><td>5</td><td>15</td><td>140</td><td>0.2910448</td><td>0.3551089</td><td>95</td><td>0.4813433</td><td>OAK</td><td>AL</td><td>120000</td></tr>
	<tr><th scope="row">7874</th><td>giambja01</td><td>1997</td><td>1</td><td>OAK</td><td>AL</td><td>142</td><td>142</td><td>519</td><td>66</td><td>152</td><td>41</td><td>2</td><td>20</td><td>81</td><td>0</td><td>1</td><td>55</td><td>89</td><td>3</td><td>6</td><td>0</td><td>8</td><td>11</td><td>142</td><td>0.2928709</td><td>0.3622449</td><td>89</td><td>0.495183</td><td>OAK</td><td>AL</td><td>205000</td></tr>
	<tr><th scope="row">7875</th><td>giambja01</td><td>1998</td><td>1</td><td>OAK</td><td>AL</td><td>153</td><td>153</td><td>562</td><td>92</td><td>166</td><td>28</td><td>0</td><td>27</td><td>110</td><td>2</td><td>2</td><td>81</td><td>102</td><td>7</td><td>5</td><td>0</td><td>9</td><td>16</td><td>153</td><td>0.2953737</td><td>0.3835616</td><td>111</td><td>0.4893238</td><td>OAK</td><td>AL</td><td>315000</td></tr>
	<tr><th scope="row">7876</th><td>giambja01</td><td>1999</td><td>1</td><td>OAK</td><td>AL</td><td>158</td><td>158</td><td>575</td><td>115</td><td>181</td><td>36</td><td>1</td><td>33</td><td>123</td><td>1</td><td>1</td><td>105</td><td>106</td><td>6</td><td>7</td><td>0</td><td>8</td><td>11</td><td>158</td><td>0.3147826</td><td>0.4215827</td><td>111</td><td>0.5530435</td><td>OAK</td><td>AL</td><td>2103333</td></tr>
	<tr><th scope="row">7877</th><td>giambja01</td><td>2000</td><td>1</td><td>OAK</td><td>AL</td><td>152</td><td>152</td><td>510</td><td>108</td><td>170</td><td>29</td><td>1</td><td>43</td><td>137</td><td>2</td><td>0</td><td>137</td><td>96</td><td>6</td><td>9</td><td>0</td><td>8</td><td>9</td><td>152</td><td>0.3333333</td><td>0.4759036</td><td>97</td><td>0.6470588</td><td>OAK</td><td>AL</td><td>3103333</td></tr>
	<tr><th scope="row">7878</th><td>giambja01</td><td>2001</td><td>1</td><td>OAK</td><td>AL</td><td>154</td><td>154</td><td>520</td><td>109</td><td>178</td><td>47</td><td>2</td><td>38</td><td>120</td><td>2</td><td>0</td><td>129</td><td>83</td><td>24</td><td>13</td><td>0</td><td>9</td><td>17</td><td>154</td><td>0.3423077</td><td>0.4769001</td><td>91</td><td>0.6596154</td><td>OAK</td><td>AL</td><td>4103333</td></tr>
	<tr><th scope="row">7879</th><td>giambja01</td><td>2002</td><td>1</td><td>NYA</td><td>AL</td><td>155</td><td>155</td><td>560</td><td>120</td><td>176</td><td>34</td><td>1</td><td>41</td><td>122</td><td>2</td><td>2</td><td>109</td><td>112</td><td>4</td><td>15</td><td>0</td><td>5</td><td>18</td><td>155</td><td>0.3142857</td><td>0.4354136</td><td>100</td><td>0.5982143</td><td>NYA</td><td>AL</td><td>10428571</td></tr>
	<tr><th scope="row">7880</th><td>giambja01</td><td>2003</td><td>1</td><td>NYA</td><td>AL</td><td>156</td><td>156</td><td>535</td><td>97</td><td>134</td><td>25</td><td>0</td><td>41</td><td>107</td><td>2</td><td>1</td><td>129</td><td>140</td><td>9</td><td>21</td><td>0</td><td>5</td><td>9</td><td>156</td><td>0.2504673</td><td>0.4115942</td><td>68</td><td>0.5271028</td><td>NYA</td><td>AL</td><td>11428571</td></tr>
	<tr><th scope="row">7881</th><td>giambja01</td><td>2004</td><td>1</td><td>NYA</td><td>AL</td><td>80</td><td>80</td><td>264</td><td>33</td><td>55</td><td>9</td><td>0</td><td>12</td><td>40</td><td>0</td><td>1</td><td>47</td><td>62</td><td>1</td><td>8</td><td>0</td><td>3</td><td>5</td><td>80</td><td>0.2083333</td><td>0.3416149</td><td>34</td><td>0.3787879</td><td>NYA</td><td>AL</td><td>12428571</td></tr>
	<tr><th scope="row">7882</th><td>giambja01</td><td>2005</td><td>1</td><td>NYA</td><td>AL</td><td>139</td><td>139</td><td>417</td><td>74</td><td>113</td><td>14</td><td>0</td><td>32</td><td>87</td><td>0</td><td>0</td><td>108</td><td>109</td><td>5</td><td>19</td><td>0</td><td>1</td><td>7</td><td>139</td><td>0.2709832</td><td>0.440367</td><td>67</td><td>0.5347722</td><td>NYA</td><td>AL</td><td>13428571</td></tr>
	<tr><th scope="row">7883</th><td>giambja01</td><td>2006</td><td>1</td><td>NYA</td><td>AL</td><td>139</td><td>139</td><td>446</td><td>92</td><td>113</td><td>25</td><td>0</td><td>37</td><td>113</td><td>2</td><td>0</td><td>110</td><td>106</td><td>12</td><td>16</td><td>0</td><td>7</td><td>10</td><td>139</td><td>0.2533632</td><td>0.4127807</td><td>51</td><td>0.558296</td><td>NYA</td><td>AL</td><td>20428571</td></tr>
	<tr><th scope="row">7884</th><td>giambja01</td><td>2007</td><td>1</td><td>NYA</td><td>AL</td><td>83</td><td>83</td><td>254</td><td>31</td><td>60</td><td>8</td><td>0</td><td>14</td><td>39</td><td>1</td><td>0</td><td>40</td><td>66</td><td>2</td><td>8</td><td>0</td><td>1</td><td>1</td><td>83</td><td>0.2362205</td><td>0.3564356</td><td>38</td><td>0.4330709</td><td>NYA</td><td>AL</td><td>23428571</td></tr>
	<tr><th scope="row">7885</th><td>giambja01</td><td>2008</td><td>1</td><td>NYA</td><td>AL</td><td>145</td><td>145</td><td>458</td><td>68</td><td>113</td><td>19</td><td>1</td><td>32</td><td>96</td><td>2</td><td>1</td><td>76</td><td>111</td><td>5</td><td>22</td><td>0</td><td>9</td><td>6</td><td>145</td><td>0.2467249</td><td>0.3734513</td><td>61</td><td>0.5021834</td><td>NYA</td><td>AL</td><td>23428571</td></tr>
	<tr><th scope="row">7886</th><td>giambja01</td><td>2009</td><td>1</td><td>OAK</td><td>AL</td><td>83</td><td>83</td><td>269</td><td>39</td><td>52</td><td>13</td><td>0</td><td>11</td><td>40</td><td>0</td><td>0</td><td>50</td><td>72</td><td>1</td><td>7</td><td>0</td><td>2</td><td>6</td><td>NA</td><td>0.1933086</td><td>0.3323171</td><td>28</td><td>0.3643123</td><td>OAK</td><td>AL</td><td>4000000</td></tr>
	<tr><th scope="row">7887</th><td>giambja01</td><td>2009</td><td>2</td><td>COL</td><td>NL</td><td>19</td><td>19</td><td>24</td><td>4</td><td>7</td><td>1</td><td>0</td><td>2</td><td>11</td><td>0</td><td>0</td><td>7</td><td>8</td><td>0</td><td>0</td><td>0</td><td>0</td><td>0</td><td>NA</td><td>0.2916667</td><td>0.4516129</td><td>4</td><td>0.5833333</td><td>OAK</td><td>AL</td><td>4000000</td></tr>
	<tr><th scope="row">7888</th><td>giambja01</td><td>2010</td><td>1</td><td>COL</td><td>NL</td><td>87</td><td>87</td><td>176</td><td>17</td><td>43</td><td>9</td><td>0</td><td>6</td><td>35</td><td>2</td><td>0</td><td>35</td><td>47</td><td>5</td><td>6</td><td>0</td><td>5</td><td>5</td><td>NA</td><td>0.2443182</td><td>0.3783784</td><td>28</td><td>0.3977273</td><td>COL</td><td>NL</td><td>1750000</td></tr>
	<tr><th scope="row">7889</th><td>giambja01</td><td>2011</td><td>1</td><td>COL</td><td>NL</td><td>64</td><td>64</td><td>131</td><td>20</td><td>34</td><td>6</td><td>0</td><td>13</td><td>32</td><td>0</td><td>0</td><td>17</td><td>45</td><td>0</td><td>3</td><td>0</td><td>1</td><td>1</td><td>64</td><td>0.259542</td><td>0.3552632</td><td>15</td><td>0.6030534</td><td>COL</td><td>NL</td><td>1000000</td></tr>
	<tr><th scope="row">7890</th><td>giambja01</td><td>2012</td><td>1</td><td>COL</td><td>NL</td><td>60</td><td>NA</td><td>89</td><td>7</td><td>20</td><td>4</td><td>0</td><td>1</td><td>8</td><td>0</td><td>0</td><td>20</td><td>24</td><td>2</td><td>2</td><td>0</td><td>2</td><td>4</td><td>NA</td><td>0.2247191</td><td>0.3716814</td><td>15</td><td>0.3033708</td><td>COL</td><td>NL</td><td>1000000</td></tr>
	<tr><th scope="row">7891</th><td>giambja01</td><td>2013</td><td>1</td><td>CLE</td><td>AL</td><td>71</td><td>71</td><td>186</td><td>21</td><td>34</td><td>8</td><td>0</td><td>9</td><td>31</td><td>0</td><td>1</td><td>23</td><td>56</td><td>0</td><td>4</td><td>0</td><td>3</td><td>8</td><td>NA</td><td>0.1827957</td><td>0.2824074</td><td>17</td><td>0.3709677</td><td>CLE</td><td>AL</td><td>750000</td></tr>
	<tr><th scope="row">20112</th><td>saenzol01</td><td>1999</td><td>1</td><td>OAK</td><td>AL</td><td>97</td><td>97</td><td>255</td><td>41</td><td>70</td><td>18</td><td>0</td><td>11</td><td>41</td><td>1</td><td>1</td><td>22</td><td>47</td><td>1</td><td>15</td><td>0</td><td>3</td><td>6</td><td>97</td><td>0.2745098</td><td>0.3627119</td><td>41</td><td>0.4745098</td><td>OAK</td><td>AL</td><td>240000</td></tr>
	<tr><th scope="row">20113</th><td>saenzol01</td><td>2000</td><td>1</td><td>OAK</td><td>AL</td><td>76</td><td>76</td><td>214</td><td>40</td><td>67</td><td>12</td><td>2</td><td>9</td><td>33</td><td>1</td><td>0</td><td>25</td><td>40</td><td>2</td><td>7</td><td>0</td><td>1</td><td>6</td><td>76</td><td>0.3130841</td><td>0.4008097</td><td>44</td><td>0.5140187</td><td>OAK</td><td>AL</td><td>260000</td></tr>
	<tr><th scope="row">20114</th><td>saenzol01</td><td>2001</td><td>1</td><td>OAK</td><td>AL</td><td>106</td><td>106</td><td>305</td><td>33</td><td>67</td><td>21</td><td>1</td><td>9</td><td>32</td><td>0</td><td>1</td><td>19</td><td>64</td><td>1</td><td>13</td><td>1</td><td>3</td><td>9</td><td>106</td><td>0.2196721</td><td>0.2911765</td><td>36</td><td>0.3836066</td><td>OAK</td><td>AL</td><td>290000</td></tr>
	<tr><th scope="row">20115</th><td>saenzol01</td><td>2002</td><td>1</td><td>OAK</td><td>AL</td><td>68</td><td>68</td><td>156</td><td>15</td><td>43</td><td>10</td><td>1</td><td>6</td><td>18</td><td>1</td><td>1</td><td>13</td><td>31</td><td>1</td><td>7</td><td>0</td><td>2</td><td>2</td><td>68</td><td>0.275641</td><td>0.3539326</td><td>26</td><td>0.4679487</td><td>OAK</td><td>AL</td><td>800000</td></tr>
	<tr><th scope="row">20116</th><td>saenzol01</td><td>2005</td><td>1</td><td>LAN</td><td>NL</td><td>109</td><td>109</td><td>319</td><td>39</td><td>84</td><td>24</td><td>0</td><td>15</td><td>63</td><td>0</td><td>1</td><td>27</td><td>63</td><td>1</td><td>3</td><td>0</td><td>2</td><td>12</td><td>109</td><td>0.2633229</td><td>0.3247863</td><td>45</td><td>0.4796238</td><td>LAN</td><td>NL</td><td>650000</td></tr>
	<tr><th scope="row">20117</th><td>saenzol01</td><td>2006</td><td>1</td><td>LAN</td><td>NL</td><td>103</td><td>103</td><td>179</td><td>30</td><td>53</td><td>15</td><td>0</td><td>11</td><td>48</td><td>0</td><td>0</td><td>14</td><td>47</td><td>1</td><td>7</td><td>0</td><td>4</td><td>4</td><td>103</td><td>0.2960894</td><td>0.3627451</td><td>27</td><td>0.5642458</td><td>LAN</td><td>NL</td><td>1000000</td></tr>
	<tr><th scope="row">20118</th><td>saenzol01</td><td>2007</td><td>1</td><td>LAN</td><td>NL</td><td>92</td><td>92</td><td>110</td><td>9</td><td>21</td><td>5</td><td>0</td><td>4</td><td>18</td><td>0</td><td>0</td><td>16</td><td>25</td><td>0</td><td>2</td><td>0</td><td>4</td><td>5</td><td>92</td><td>0.1909091</td><td>0.2954545</td><td>12</td><td>0.3454545</td><td>LAN</td><td>NL</td><td>1000000</td></tr>
</tbody>
</table>

</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p>Since all these players were lost in after 2001 in the offseason, let's only concern ourselves with the data from 2001.</p>
<p><strong> Use subset again to only grab the rows where the yearID was 2001. </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[20]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>lost_players <span class="o">&lt;-</span> <span class="kp">subset</span><span class="p">(</span>lost_players<span class="p">,</span>yearID <span class="o">==</span> <span class="m">2001</span><span class="p">)</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Reduce the lost_players data frame to the following columns: playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB</strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[21]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>lost_players <span class="o">&lt;-</span> lost_players<span class="p">[,</span><span class="kt">c</span><span class="p">(</span><span class="s">'playerID'</span><span class="p">,</span><span class="s">'H'</span><span class="p">,</span><span class="s">'X2B'</span><span class="p">,</span><span class="s">'X3B'</span><span class="p">,</span><span class="s">'HR'</span><span class="p">,</span><span class="s">'OBP'</span><span class="p">,</span><span class="s">'SLG'</span><span class="p">,</span><span class="s">'BA'</span><span class="p">,</span><span class="s">'AB'</span><span class="p">)]</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[22]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="kp">head</span><span class="p">(</span>lost_players<span class="p">)</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[22]:</div>

<div class="output_html rendered_html output_subarea output_execute_result">
<table>
<thead><tr><th></th><th scope="col">playerID</th><th scope="col">H</th><th scope="col">X2B</th><th scope="col">X3B</th><th scope="col">HR</th><th scope="col">OBP</th><th scope="col">SLG</th><th scope="col">BA</th><th scope="col">AB</th></tr></thead>
<tbody>
	<tr><th scope="row">5141</th><td>damonjo01</td><td>165</td><td>34</td><td>4</td><td>9</td><td>0.3235294</td><td>0.363354</td><td>0.2562112</td><td>644</td></tr>
	<tr><th scope="row">7878</th><td>giambja01</td><td>178</td><td>47</td><td>2</td><td>38</td><td>0.4769001</td><td>0.6596154</td><td>0.3423077</td><td>520</td></tr>
	<tr><th scope="row">20114</th><td>saenzol01</td><td>67</td><td>21</td><td>1</td><td>9</td><td>0.2911765</td><td>0.3836066</td><td>0.2196721</td><td>305</td></tr>
</tbody>
</table>

</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<h2 id="Replacement-Players">Replacement Players<a class="anchor-link" href="#Replacement-Players">¶</a></h2><p>Now we have all the information we need! Here is your final task - Find Replacement Players for the key three players we lost! However, you have three constraints:</p>
<ul>
<li>The total combined salary of the three players can not exceed 15 million dollars.</li>
<li>Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.</li>
<li>Their mean OBP had to equal to or greater than the mean OBP of the lost players</li>
</ul>
<p><strong> Use the combo dataframe you previously created as the source of information! Remember to just use the 2001 subset of that dataframe. There's lost of different ways you can do this, so be creative! It should be relatively simple to find 3 players that satisfy the requirements, note that there are many correct combinations available!</strong></p>
<p><a href="http://www.cookbook-r.com/Manipulating_data/Sorting/">Helpful info on sorting data frames</a></p>

</div>
</div>
</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<hr>
<h2 id="Example-Solution">Example Solution<a class="anchor-link" href="#Example-Solution">¶</a></h2><p><strong> Note: There are lots of correct answers and ways to solve this!</strong></p>
<p><strong> First only grab available players from year 2001 </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[38]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="kn">library</span><span class="p">(</span>dplyr<span class="p">)</span>
avail.players <span class="o">&lt;-</span> filter<span class="p">(</span>combo<span class="p">,</span>yearID<span class="o">==</span><span class="m">2001</span><span class="p">)</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Then I made a quick plot to see where I should cut-off for salary in respect to OBP:</strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[39]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre><span class="kn">library</span><span class="p">(</span>ggplot2<span class="p">)</span>
ggplot<span class="p">(</span>avail.players<span class="p">,</span>aes<span class="p">(</span>x<span class="o">=</span>OBP<span class="p">,</span>y<span class="o">=</span>salary<span class="p">))</span> <span class="o">+</span> geom_point<span class="p">()</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt"></div>
<div class="output_subarea output_stream output_stderr output_text">
<pre>Warning message:
: Removed 168 rows containing missing values (geom_point).</pre>
</div>
</div>

<div class="output_area"><div class="prompt"></div>


<div class="output_png output_subarea ">
<img src="data:image/png;base64,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NwIMsfPGkVwQQAABXwi8++67CW/ompeXZ3PnzvVFOctbiDfeeKOw56jocxQkTZs2regmfkcAAQQQQMATAQIkTxjJBAEEEPCHgO4SXlYqekPWso7zy77s7MQfU8nq6pc6UA4EEEAAgWAJJP7kCVY9KC0CCCCAwG8CBx98cNweF+HUqlXL2rZtGyin7t27W05OTqkya5tuKEtCAAEEEEDAawECJK9FyQ8BBBDIoMAOO+xgN954o6nnRavSKamnRf/uvPPOhMPvMljkMk+teVOqU9EgSb936NDBTjnllDKfy04EEEAAAQS2RYBFGrZFjecggAACPhY488wzbbfddrOHH37Yvv32W9PKdJdffrm1aNHCx6WOX7SGDRva9OnTTSvX6Wf16tXtuOOOs9NPP92CNlwwfg3ZigACCCDgNwECJL+1COVBAAEEPBDo0qWL6Z+Selw0vG7lypUe5Jz+LOrVq2dXXHGF+5f+s3NGBBBAAIGoCTDELmotTn0RQAABBBBAAAEEEEAgoQABUkIadiCAAAIIIIAAAggggEDUBAiQotbi1BcBBBBAAAEEEEAAAQQSChAgJaRhBwIIIIAAAggggAACCERNgAApai1OfRFAAAEEEEAAAQQQQCChAAFSQhp2IIAAAggggAACCCCAQNQEUgqQtm7davvtt5+NHj3alixZEjU76osAAggggAACCCCAAAIhE0gpQJJFbm6uDR482Jo2bWq9e/e2Z5991vLy8kLGRHUQQAABBBBAAAEEEEAgCgIpBUhZWVn23nvv2fz5890N/D766CM78cQTbaeddrL/+7//s9mzZ0fBkDoigAACCCCAAAIIIIBASARSCpBiBnvuuaeNHDnSvv32W5s+fbr16tXLnnzySevQoYO1a9fObr/9dvv1119jh/MTAQQQQAABBBBAAAEEEPClgCcBUqxm6lE64ogj7K677rJ77rnHdt99d/vkk0/ssssuc0PwzjzzTFu5cmXscH4igAACCCCAAAIIIIAAAr4S8CxA2rx5s02ZMsUGDBhgjRo1sn79+tmWLVtsxIgRpqF3119/vT333HPWuXNn27Rpk68QKAwCCCCAAAIIIIAAAgggIIGqqTJoDtK///1ve+qpp9wwutq1a9vJJ59sAwcOtK5duxZmr9XuWrRoYX/5y19s5syZrqepcCe/IIAAAhEX2LBhgy1cuNBq1aplu+66a8Q1qD4CCCCAAAKZE0gpQNIy3506dTINrVMwdOutt1rfvn3dB3y8KjVu3Njq1atnCqJICCCAAAL/E3jwwQfdPM78/HzX896qVSvTtr333hsiBBBAAAEEEEizQEoBksp63nnnuTlGmm+ULB155JHMQUqGxH4EEIiUwPjx4+3GG2+0goKCwnp/8803dsIJJ9isWbOsYcOGhdv5BQEEEEAAAQQqXyClOUj6tlND6/ThXp6kniYSAggggMAfAup5LxocaY965zVXc9y4cX8cyG8IIIAAAgggkBaBlAKkH3/80VavXm0777xzWgrLSRBAAIEwCSgwWrx4cdwq6YbbCxYsiLuPjQgggAACCCBQeQIpBUgaJ3/GGWe4bzk//PDDyislOSOAAAIhFKhSpYrVrVs3bs1ycnLc7RHi7mQjAggggAACCFSaQEpzkDQMRMt7r1u3zg444ACrU6eOtWzZ0vTBXjQNGjTIzVUquo3fEUAAAQTMrfip+8ZpyHLRpNsk6HYJJAQQQAABBBBIr0BKAZKKOm/ePGvevLn7Fyu6PtiLJgVSJAQQQACB0gKDBw+2L7/80l566SWrVq2am3+k99A777zTdtttt9JPYAsCCCCAAAIIVKpASgGSFl3QTWBJCCCAAALbJqAedy3pPXfuXNNQZd0G4YgjjmD1um3j5FkIIIAAAgikLJBSgFSesy9dutTdQHavvfYqz+EcgwACCERSoF27dqZ/JAQQQAABBBDIrEBKizSo6N9//70NGDDA3fm9SZMmtuOOO7p/O+ywg5t8rJ+6zwcJAQQQQAABBBBAAAEEEPC7QMoB0sCBA+3JJ5909+zQ0BDdu0M3jc3OzrY1a9ZYjx49mGjs91cB5UMAAQQQQAABBBBAAAEnkFKAtH79envttddMq9R99913dtNNN5kmF0+fPt1++eUXu/rqq23hwoW2yy67wI0AAggggAACCCCAAAII+F4gpQBp0aJFbsWlvn37mhZsaN++vbtx7Pz5813FR44caVWrVrUxY8b4HoICIoAAAggggAACCCCAAAIpBUjbbbedE6xRo4b7qRvH5ubmFlvZ7k9/+pNbmQlqBBBAAAEEEEAAAQQQQMDvAikFSE2bNnU3h500aZKrp+4Kr/lHb7zxRmG9dZ+k6tWrFz7mFwQQQAABBBBAAAEEEEDArwIpL/OtmxwOGzbMFixYYM8//7z16tXLbrnlFmvcuLH99NNP9v7777u5SX4FoFwIIIAAAggggAACCCCAQEwg5QBpyJAhtmHDBvvkk09cnldccYVbpGHUqFHu8QEHHGCHHXZY7Hz8RAABBEIlMGPGDBsxYoRbkKZu3brWv39/0/tgtWrVQlVPKoMAAggggEBUBFIOkHQXePUYbd261ZnVr1/fZs6c6eYdacnvzp07u4UaogJKPRFAIDoCU6ZMsfPOO8+t3qlar1ixwh588EGbO3euTZgwIToQ1BQBBBBAAIEQCaQcIMUstIpdLGku0oEHHhh7yE8EEEAglALXXHNNYXAUq2B+fr69++67pp6lww8/PLaZnwgggAACCCAQEIEKB0jDhw+3yZMnV6h655xzjukfCQEEEAiLwOLFi23JkiVxq6MvjN577z0CpLg6bEQAAQQQQMDfAhUOkPLy8mzdunUVqpW+TOZouwAAQABJREFUUSUhgAACYRKI3d4gXp2ys7OtZs2a8XaxDQEEEEAAAQR8LlDhAEk9SPpHQgABBKIsoPmWHTp0cPd927JlSzEKfZHUvXv3Ytt4gAACCCCAAALBEEjpPkjlqeLSpUtt/vz55TmUYxBAAIFACdx5551Wr149d4NsFVzzLzW8bujQobbbbrsFqi4UFgEEEEAAAQT+J1DhHqSScN9//71deeWVbry9ht7FVrMrKCgwrWK3Zs0au/766+2GG24o+VQeI4AAAoEWaNWqlb399tv2+OOPu56kRo0aWd++fa1jx46BrheFRwABBBBAIMoCKQdIAwcOdPc9atasmdWuXdvUY9S2bVv7/PPPXXDUo0cP69evX5SNqTsCCIRYYLvttrNLLrkkxDWkaggggAACCERLIKUhduvXr7fXXnvNBg0aZN99953ddNNNbsnb6dOn2y+//GJXX321u3niLrvsEi1VaosAAggggAACCCCAAAKBFEgpQFq0aJEbUqchJRp33759e1u9enXhnKORI0e6m8SOGTMmkDgUGgEEEEAAAQQQQAABBKIlkFKApKElSrHlbjUePzc3143FjzH+6U9/sg8//DD2kJ8IIIAAAggggAACCCCAgG8FUgqQmjZtanXq1LFJkya5CmoFp913393eeOONwgrPmzfPqlevXviYXxBAAAEEEEAAAQQQQAABvwqkvEjD4MGDbdiwYbZgwQJ7/vnnrVev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YPatt94yzavU4h+J2tXLQulLq0QL4GjYne5XR0IAAQQQQCAIAoEZYqdvtjWfSCvSaZicbtZ6yimnuHk9Gp6knqOhQ4cWTpodOHCgG06ibepO05C8/v37u+eXbJhE903ShYbmGBx33HFusYZ999231L1qFCDpm+L69euXzJbHPhTQYgNDhgwpXExDwxA0LOn//u//0lpaBRwtW7ZM6znLczL1tmpZfH2pUDIlWqGx5HFReqwvZLRKm3oF1UMTG46h+7HpPShTSWVp3bp1Wk+v17TuE6fVQuWgf0oKFk877TTbe++901oeToYAAggggMC2CmT99iFWvvXutvUMlfw8fTOpeUGJvrnUWEetHFbWUJhkRVT+CrI0uczLlOk5SJoErzppMmcUxiurx0Y30Sw51Edte++995bqRaxoW2sypwKLoiuaVTQPPxyve5FpFcjq1au7i1y9NnQz23Re8GvOir7U0N+e5hT6PWke42uvvebGqqunRF/g+CnpSyT9rWtBlspOumH2P//5T9e7r/ddvW70ZVaYkuahajGWRKtShqmu6aiLXiexW3Sk43xhPofeN/X+menri7AY68tvfQZF4RqpsttMn0G67tT7ZlmLWVV2OXTNV56YIPABUmVDVmb+mX4Di1qApAs1rTgY7zuB9u3b24svvphSc4clQBLCd99954YA6sJaF/yNGzdOyaaiTw5agFTR+qX7+HQGSOmuWybOR4DkrToBkneeBEjeWSonAiTvPIMWIAVmiJ13TUROURXQcMh4wZE8dC8s0h8Cmm/EnKM/PPgNAQQQQAABBKIjEJhFGqLTJNS0sgQ01yzehHqdL94y7JVVDvJFAAEEEEAAAQQQ8K8AAZJ/24aSeSxwwQUXxM1RQdMll1wSdx8bEUAAAQQQQAABBKIlQIAUrfaOdG21CuEDDzzgVjrUylr6p4UINKGc+1hF+qVB5RFAAAEEEEAAgUIB5iAVUvBLFAS0LPMRRxzh7kOkFef2228/002Fo550n69PPvnEBY26J5OCRxICCCCAAAIIIBBFAa6CotjqEa+zeo06deoUcYU/qj9hwgR3E2Utma9FLHST5LvuussFkn8cxW8IIIAAAggggEA0BBhiF412ppYIxBV4/fXXbfDgwe7eTepR0z2iVqxY4W72OX/+/LjPYSMCCCCAAAIIIBBmAQKkMLcudYusQMmb4SaCGD16dKkb58aOve+++2K/8hMBBBBAAAEEEIiMAAFSZJqaioZdYPPmzXbrrbfaXnvtZc2aNbMDDzzQnn766TKr/dVXX8Xdr94kepDi0rARAQQQQAABBEIuwBykkDcw1YuOwIUXXmivvPKK5efnu0rr5reXXnqprV692s4666y4EE2aNLHly5eX2peVlWUtWrQotZ0NCCCAAAIIIIBA2AXoQQp7C1O/SAjMnTvXpkyZUhgcxSqtnqARI0aYVqmLl84//3yrUqVKvF0Jg6q4B7MRAQQQQAABBBAIiQABUkgakmpEW+CDDz6w3NzcuAhanW7hwoVx95100kl20UUXmXqMtLqf/mmJ75tvvtkOOuiguM9hIwIIIIAAAgggEGYBhtiFuXWT1E1Dr3799Vd3UZzkUHb7XKB27doJS6gFG8q619NVV11lp556qr3zzjuuN6lr167WqFGjhPmxAwEEEEAAAQQQCLMAAVKYWzdB3ZYtW+aWdp42bZq7702tWrXsiiuusPPOOy/BM9jsdwHd/FbD6Uom9Qy1bNnSdt1115K7ij3eeeedTf9ICCCAAAIIIIBA1AUYYhexV4Auovv06WMzZsxwwZGqv27dOjdP5YEHHoiYRniqu/3229sdd9xh2dnZlpOT4yqmIXcKfmnX8LQzNUEAAQQQQACByhegB6nyjX11Bq1ytmjRItOS0EVTbIlorXamOSik4Akcf/zxtvfee9v48eNNK9jpdw2dU/BEQgABBBBAAAEEECifAFfC5XMKzVHz5s1LWJf169fbjz/+yPLOCYX8v2P33Xe3a6+91v8FpYQIIIAAAggggIBPBRhi59OGqaxiNWzYMOGyzjrndtttV1mnJl8EEPhdQPMAZ82axc14eUUggAACCCDgQwECJB82SmUW6Zhjjok7mV/D6jTRv27dupV5evJGINICWlHwuuuus3333df69+9vRx55pGnVwC+++CLSLlQeAQQQQAABPwkQIPmpNdJQlsaNG9v999/vJvLrnjeayK9/rVq1cpP801AEToFAZAX++c9/2mOPPWYKlPLz891CKV9//bXpflQa4kpCAAEEEEAAgcwLMAcp822Q9hJ0797d/vvf/9r06dNt1apVLjjq1q0bizOkvSU4YVgEtDpklSpVyqyOFkK59957XWBU9EAFS2vWrLFJkybZgAEDiu7y5HflHy9p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g+b+jB8/3t0UtlGjRnGBFMxo5bt4Sc9fvnx5vF1xt3Xq1MktGqF5Uzqf5kcNHTrU7r///rjHl3ejVsK755573BcE6tkiIYAAAgggEDQBAqSgtRjl9ZWAFmMo+g18rHC6iO3QoUPsIT8jIqDFGOLNL1LPTHleD7m5ufa3v/3NdK8lLf2tvKpXr27VqlVzrzMt+KDeqERJQU5FUps2beyxxx5z53v99dft3HPPjft6Lk+e69evt5NOOsn69+/v5l1de+211rFjR4aalgePYxBAAAEEfCVAgOSr5qAwQRO44oorSl0Q66JWF8RavIEULQHddLVkwBx7PWjxhlgqz4IMCjA0n+nyyy83LcmthSEeffRR0w1nS85V0uPzzjuvzPswxc5dWT+vv/56++CDD9y8KM2P0hLimh91/vnn23fffVdZpyVfBBBAAAEEPBcgQPKclAyjJKAeJA2Lat68eWG199hjD7fCWdFthTv5JdQCmkOkuWfNmjUrrOfee+/tluTW0txa9e6II46wFi1auH//n703AbtqbPv/r+f3f4/jxUuG8kSikgghGTM1GBpMEckcRSKFDA2GKDKVHpShJBLJFIoiVCqKigYzTfJECOHxvsfxHu///lye67buta+17z2se4/f8zjue++9hmtd13ettff1Xed5fk88NsnC0NgXBbzLL7+8Ujoejw/1lgipw/AwsT6okld58By9IQcK+XJfPh79fP7553PUEx1GCAgBISAEhED2CPytQmHp/7JvRi1kgsA///nPTHaLbZ9atWoZJIu///5778QmtgOVSUO//fabHSmYyrJDgJAylNZ++eUX43DNrsXc7r1q1Sor9c04OnToYOrWrWsWLFhgTjvtNCvg4XpDKCbrZs+ebeXC3fJUXlGy++GHH+z+YY9SeH/Wc13+9NNPdtW6detsnSa+/pEnDxK68L6pfKau0p577hm5abeKGlG33XZb5PpiWvH5558bamCBKWGTtWvXLqbuF2RfyYFTzmY8p4bvTb538j2/iGc0+W9lm222sb9Bvoc/+e9dcfWA3yDmneTKEmWQL+OhXVSeb7BP/mzf4BZ6LwSEQEoIMNHlSToTV1n5InDLLbdYoQMmKRihckiBP/nkk1XIEeuQ6caD9MQTT9gQOZalahSGzYTY3H///bZuEv2DIBEGR6ho3759Uz10wnb0BcLgm0SQ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6KO26Gg1uxfd6nr16tnwJvekPWoAuSi0CjmisCs1gVx/yCWZM2eOef3110vWixeFuZYbQ+FVromffvqpUgUOEjRr1iybGxeWEqfArM/wglKEtqYMeW8eIlAXhLy9KLGGmjq+2hUCQkAICAEhkCsElIOUK6QL5DjvvvuuDfsJ11xhcnXNNdcUSC/j7QYJ88nC6zgaXpluNVio040Iz1GQHLGcc4G8+mOPPeY2i+0V2eXly5ebd955xyDRnI/ixLENpkQbuvfeey2xCUpk855co5EjRyaMep999klYxgJygWoiDw0iP2DAAEMo3UUXXWTD6RA7wTMrEwJCQAgIASFQigiIIJXiWU0yJtSuop78IlaADHapGflWhC+RB7LZZptVjt995vXss882Z555Zo0PnbA65zkKHgxVsrfeeiu4KOv3r7zyiq31REI/OWaojB1wwAHmrLPOsqGVWR9ADcSCAOc9/MCChlmGFylsN998c+U17NZB8OvWrWvPrVsW1+uIESOsYh5hqI7cr1+/3px++unW6xXXcdSOEBACQkAICIFCQUAhdoVyJnLUD1SzogrF0gXWl6LhHSJJntBCinISdrdhwwbrWUJtbO+9987JsJ16me9gkLe4jLBJQil9nrO5c+eac88910ydOjWuw6mdLBBIds/51h100EFWve766683yNcjjtC+fXub17bFFltk0ZPEXbl+HnrooQQCB1mC1CM20b1798QdtUQICAEhIASEQBEjIIJUxCcvk66T64A3JWyQpqZNm1q57/C6UvmMxPkFF1yQ1+F07NjRPPXUUwkTTjwAJ554Ymx9u+eee7zkiAMQvvXhhx9aFTxCpWT5ReDkk082n332WcI1AfFhnc+oWUbOGt7IKI+wb790lyHiwgMFn0GQKLYsEwJCQAgIASFQaggoxK7Uzmg140Gtbscdd0zYiifCTN5lNYtA//79Tf369Svr4HA0yBHFawmDi8s+/fTTpE3hyfriiy+SbqOVuUGAvJ5mzZpVkcmGHO21116mZ8+eSTtRk+SIAyPIEOXZ5BriWpYJASEgBISAECg1BORBKrUzWs14kBEmtMxnY8aMMVdeeaXN1fGt17LsEWDCyZP/8RW1mcgvIYQKzxEqguRCxWXIQ5MnEmV4kXwS0lHba3nNIcA18MILL1jPIjWuMPLGyItLFpJZcz36q2UIGF5X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kn0WY/JMiHp8O4YPHx7ZB50UAkEIxM0l/3udOvDDbNeunRk3blyjKtFuTp48OZsygV17guC0adOmUVkdEAJJESDyLO9g94zkuqC5GfesjTuftD9plBNBSgPFCqsDPyEc3pIGU4grj9aIMl4fkAobsrpTJQjEzbVch4H2iLDzzE2vXX2u9ai8EMj1OZdk7mEqigafgDcSIVAIAmnPJXw82fj0my8X0kddKwSSIBD3rI07n6SNNMqIIKWBouoQAkJACAgBISAEhIAQEAJCoCYQ0LZWTdxGDUIICAEhIASEgBAQAkJACAiBNBAQQUoDRdUhBISAEBACQkAICAEhIASEQE0gIIJUE7dRgxACQkAICAEhIASEgBAQAkIgDQREkNJAUXUIASEgBISAEBACQkAICAEhUBMIiCDVxG3UIISAEBACQkAICAEhIASEgBBIA4Fvp1GJ6hACQkAICAEhUG0IkAbwrbfessmwt9xyS7PLLruY73//+42GQUjv999/v8HxTTbZxLRq1cqGSm5wIvOFBLHr1q3zHzabb765adasWeA1jQrrgBAQAkJACJQNAYX5Lhv0algICAEhIATKgQDEaNq0aTZB5oYNG7JdIH9cv379zMyZM23+IneC5Jo777yz+5r9Sx4ZEnKeeOKJ5tRTT80e/+STTwKJFgUgViSenThxoundu3f2Gn0QAkJACAiBykFAGqTKuRfqiRAQAkJACBQZgb/97W+mV69e5oknnjBdu3Y1o0aNsuRnxYoV5ne/+5255557zJo1a8zChQtNkyZNGvQGYjNw4EB77B//+If585//bObPn2/GjRtnPv/8c5s53nvBPvvsY4YOHZo99Je//MW8++675sYbbzT9+/c3jz/+uDnooIOy5/VBCAgBISAEKgMBaZAq4z6oF0JACAgBIVACBC699FKrvTn77LPNlClTrEbH2+zs2bPN8OHDTffu3c1DDz1kNttsM+M0SGPHjjXTp0/3Fjd//etfza677mogP++9956tz2mQBg8ebObNm9egPF8eeeQR07NnTzNgwABz9913NzqvA0JACAgBIVBeBBSkobz4q3UhIASEgBAoEQIff/yxueiii0yHDh2seR3mbn4ZNmyYNZdDg/TYY4/5Tzf6DoHq2LGjoe6vv/660fmgA4ceeqjB52nZsmVBp3VMCAgBISAEyoyACFKZb4CaFwJCQAgIgdIgsHTpUvPll19aDRH+Q2Fy1FFH2VPPP/98WJHs8Q8++MAsXrzYmuttscUW2eNRH9544w3zxRdfhPopRV2rc0JACAgBIVB8BOSDVHyM1YIQEAJCQAhUAAJvvvmm7cUOO+wQ2Zu99trLfOtb3zJLlixpUA6tEj5LCJHt0Bo988wzpn379oGmdA0u/ueXDz/80GDeh/Tt2/efR/VHCAgBISAEKgkBEaRKuhvqixAQAkJACBQNgbVr19q6f/CDH0S2gSZo2223Na+99lqDcqtXr86G+yaUt4uA980339jgC/4Q4ffff79p06ZNtg7KE9gBIYLd+PHjs+f0QQgIASEgBCoHARGkyrkX6okQEAJCQAgUEQEXqhstTpQ47RC+Sl5Be+QN0oDP0SuvvGJGjhxpOnfubDDh23vvvbOXtGjRwnTq1Cn7nRxI22+/vT2GH5JECAgBISAEKhMBEaTKvC/qlRAQAkJACKSMQJcuXWyNr7/+ujniiCNCa1+1apU1oXOEKqwgiV8hRpAmot7dddddDQjSgQcemNj0LqwNHRcCQkAICIHSI6AgDaXHXC0KASEgBIRAGRDYbbfdDFqc6667znz11VehPZg6dao9N2jQoNAy3hNt27a1XyFWEiEgBISAEKh+BESQqv8eagRCQAgIASGQAAECL8yaNcv88Y9/NCeccEJgWO45c+aYW2+91RDJLmkQhZtuusm2jjZJIgSEgBAQAtWPgEzsqv8eagRCQAgIASGQEIH+/fubGTNmmBNPPNG8+OKL5thjjzVolkjy+vDDD5vf/va3pk+fPua2224zm266aYNaiVjnDaxActiXXnrJEA6cSHbUKRECQkAICIHqR0AEqfrvoUYgBISAEBACOSCA9uh73/uewZTu3HPPzV5JUAUIEMlkg/IkLV++3PDPSfPmzc12221nJkyYYE477TTTpEkTd0p/hYAQEAJCoIoR2GRjRqq4/+q6EBACQkAICIG8EVi/fr1Zt26d2WqrrUzr1q0baY3yrlgXCgEhIASEQNUiIIJUtbdOHRcCQkAICAEhIASEgBAQAkIgbQQUpCFtRFWfEBACQkAICAEhIASEgBAQAlWLgAhS1d46dVwICAEhIASEgBAQAkJACAiBtBEQQUobUdUnBISAEBACQkAICAEhIASEQNUiIIJUtbdOHRcCQkAICAEhIASEgBAQAkIgbQREkNJGVPUJASEgBISAEBACQkAICAEhULUIiCBV7a1Tx4WAEBACQkAICAEhIASEgBBIGwERpLQRVX1CQAgIASEgBISAEBACQkAIVC0CIkhVe+vUcSEgBISAEBACQkAICAEhIATSRkAEKW1EVZ8QEAJCQAgIASEgBISAEBACVYuACFLV3jp1XAgIASEgBISAEBACQkAICIG0Efhfa8tIsGdg0G0AAAAASUVORK5CYII=">
</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Looks like there is no point in paying above 8 million or so (I'm just eyeballing this number). I'll choose that as a cutt off point. There are also a lot of players with OBP==0. Let's get rid of them too.</strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[41]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>avail.players <span class="o">&lt;-</span> filter<span class="p">(</span>avail.players<span class="p">,</span>salary<span class="o">&lt;</span><span class="m">8000000</span><span class="p">,</span>OBP<span class="o">&gt;</span><span class="m">0</span><span class="p">)</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> The total AB of the lost players is 1469. This is about 1500, meaning I should probably cut off my avail.players at 1500/3= 500 AB. </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[42]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>avail.players <span class="o">&lt;-</span> filter<span class="p">(</span>avail.players<span class="p">,</span>AB <span class="o">&gt;=</span> <span class="m">500</span><span class="p">)</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Now let's sort by OBP and see what we've got! </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[44]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>possible <span class="o">&lt;-</span> <span class="kp">head</span><span class="p">(</span>arrange<span class="p">(</span>avail.players<span class="p">,</span>desc<span class="p">(</span>OBP<span class="p">)),</span><span class="m">10</span><span class="p">)</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Grab columns I'm interested in:</strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[45]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>possible <span class="o">&lt;-</span> possible<span class="p">[,</span><span class="kt">c</span><span class="p">(</span><span class="s">'playerID'</span><span class="p">,</span><span class="s">'OBP'</span><span class="p">,</span><span class="s">'AB'</span><span class="p">,</span><span class="s">'salary'</span><span class="p">)]</span>
</pre></div>

</div>
</div>
</div>

</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[46]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>possible
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[46]:</div>

<div class="output_html rendered_html output_subarea output_execute_result">
<table>
<thead><tr><th></th><th scope="col">playerID</th><th scope="col">OBP</th><th scope="col">AB</th><th scope="col">salary</th></tr></thead>
<tbody>
	<tr><th scope="row">1</th><td>giambja01</td><td>0.4769001</td><td>520</td><td>4103333</td></tr>
	<tr><th scope="row">2</th><td>heltoto01</td><td>0.4316547</td><td>587</td><td>4950000</td></tr>
	<tr><th scope="row">3</th><td>berkmla01</td><td>0.4302326</td><td>577</td><td>305000</td></tr>
	<tr><th scope="row">4</th><td>gonzalu01</td><td>0.4285714</td><td>609</td><td>4833333</td></tr>
	<tr><th scope="row">5</th><td>thomeji01</td><td>0.4161491</td><td>526</td><td>7875000</td></tr>
	<tr><th scope="row">6</th><td>alomaro01</td><td>0.4146707</td><td>575</td><td>7750000</td></tr>
	<tr><th scope="row">7</th><td>edmonji01</td><td>0.4102142</td><td>500</td><td>6333333</td></tr>
	<tr><th scope="row">8</th><td>gilesbr02</td><td>0.4035608</td><td>576</td><td>7333333</td></tr>
	<tr><th scope="row">9</th><td>pujolal01</td><td>0.402963</td><td>590</td><td>200000</td></tr>
	<tr><th scope="row">10</th><td>olerujo01</td><td>0.4011799</td><td>572</td><td>6700000</td></tr>
</tbody>
</table>

</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p><strong> Can't choose giambja again, but the other ones look good (2-4). I choose them! </strong></p>

</div>
</div>
</div>
<div class="cell border-box-sizing code_cell rendered">
<div class="input">
<div class="prompt input_prompt">In&nbsp;[47]:</div>
<div class="inner_cell">
    <div class="input_area">
<div class=" highlight hl-r"><pre>possible<span class="p">[</span><span class="m">2</span><span class="o">:</span><span class="m">4</span><span class="p">,]</span>
</pre></div>

</div>
</div>
</div>

<div class="output_wrapper">
<div class="output">


<div class="output_area"><div class="prompt output_prompt">Out[47]:</div>

<div class="output_html rendered_html output_subarea output_execute_result">
<table>
<thead><tr><th></th><th scope="col">playerID</th><th scope="col">OBP</th><th scope="col">AB</th><th scope="col">salary</th></tr></thead>
<tbody>
	<tr><th scope="row">2</th><td>heltoto01</td><td>0.4316547</td><td>587</td><td>4950000</td></tr>
	<tr><th scope="row">3</th><td>berkmla01</td><td>0.4302326</td><td>577</td><td>305000</td></tr>
	<tr><th scope="row">4</th><td>gonzalu01</td><td>0.4285714</td><td>609</td><td>4833333</td></tr>
</tbody>
</table>

</div>

</div>

</div>
</div>

</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">
<p>Great, looks like I just saved the 2001 Oakland A's a lot of money! If only I had a time machine and R, I could have made a lot of money in 2001 picking players!</p>

</div>
</div>
</div>
<div class="cell border-box-sizing text_cell rendered">
<div class="prompt input_prompt">
</div>
<div class="inner_cell">
<div class="text_cell_render border-box-sizing rendered_html">

</div>
</div>
</div>
    </div>
  </div>


