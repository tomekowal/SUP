%% Author: gosia
%% Created: 2011-07-28
%% Description: TODO: Add description to yaws_test
-module(yaws_test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, header/1, footer/0]).

%%
%% API Functions
%%

start() ->
	list_db:start_link().

header(Title) ->
    {html,[	
		<<"	<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
			<html xmlns=\"http://www.w3.org/1999/xhtml\">
			<head>
			<meta name=\"keywords\" content=\"\" />
			<meta name=\"description\" content=\"\" />
			<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
			<title>Cool name - ">>, 
				Title,
		<<"	</title>
			<link href=\"/style.css\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />
			</head>
				<body>
					<div id=\"header\">
					<div id=\"logo\">
						<h1><a href=\"#\">Cool name</a></h1>
						<p>cool motto</a></p>
					</div>
					</div>
					<div id=\"wrapper\">
						<div id=\"menu\">
							<ul>
								<li><a href=\"/\">Homepage</a></li>
								<li><a href=\"devices\">Devices</a></li>
							</ul>
						</div>
						<div id=\"search\" >
							<form method=\"get\" action=\"#\">
								<div>
									<input type=\"text\" name=\"s\" id=\"search-text\" value=\"\" />
								</div>
							</form>
						</div>
					</div>
					<div id=\"page\">
					<div id=\"content\">
		 				<h2 id=\"=title\">">>,
		  					Title,
		<<"				</h2>">>]}.

footer() ->
	{html,[	
		<<"			</div>

					<div id=\"sidebar\">
						<ul>
							<li>
								<h2>Category 1</h2>
								<p>Mauris vitae <a href=\"#\">Aliquam libero</a> nisl nec metus placerat perdiet est. Phasellus dapibus semper consectetuer hendrerit.</p>
							</li>
							<li>
								<h2>Category 2</h2>
								<ul>
									<li><a href=\"#\">Aliquam libero</a> more</li>
									<li><a href=\"#\">Consectetuer adipiscing elit</a></li>
									<li><a href=\"#\">Metus aliquam pellentesque</a></li>
									<li><a href=\"#\">Suspendisse iaculis mauris</a></li>
									<li><a href=\"#\">Urnanet non molestie semper</a></li>
									<li><a href=\"#\">Proin gravida orci porttitor</a></li>
								</ul>
							</li>
						</ul>
					</div>
					<div style=\"clear: both;\">&nbsp;</div>
				</div>
				<div id=\"footer\">
					<p>That's it! <a href=\"#\">Authors</a>.</p>
			</body>
		</html>">>]}.

	
	

%%
%% Local Functions
%%

