function inc(filename)
{
	var head = document.getElementsByTagName('head').item(0);
	var js = document.createElement('script');
    js.setAttribute('language', 'javascript');
 	js.setAttribute('type', 'text/javascript');
	js.setAttribute('src', "scripts/" + filename);
	head.appendChild(js)
}

