function addGraph(tag, svg_text) {
	var svg = $('<div class="graph">' + svg_text + '</div>');
	//svg.css({'display':'none'});
	svg.find('svg').attr({'width':'50pt','height':'50pt'});
	$(tag).append(svg);
	//svg.fadeIn();
	
	//$('html,body').animate({scrollTop: svg.offset().top}, 'slow');
	
	return svg;
}

function addRule(tag, name, lhs_svg, rhs_svg) {
    var rule = $('<div class="rule"><div class="rule_label">' + name + '</div>' +
		 '<div class="graph">' + lhs_svg + '</div>' +
	         '<div class="rule_arrow">&#x2192;</div>' +
	         '<div class="graph">' + rhs_svg + '</div>' +
	         '<div style="clear:both"></div></div>');
    rule.find('svg').attr({'width':'50pt','height':'50pt'});
    //if (!cong) rule.find('.rule_label').css({'font-weight':'bold'});
    $(tag).append(rule);
    return rule;
}

function addContainer (tag, title, expanders) {
	var container = $('<div class="container"><div class="title"><a href="#">'+title+'</a></div>'+
	                  '<div class="content"></div><div style="clear:both"></div><div class="footer"></div>');
	container.css({'display':'none'});
	
	var link = container.find('div.title a');
	var content = container.find('div.content');
	if (expanders) {
		var exps = $('<div><a id="expand" href="#">expand all</a> | <a href="#" id="collapse">collapse all</a></div>');
		content.append(exps);
		exps.find('#expand').click(function() { content.find('> .container > .content').slideDown(); return false; });
		exps.find('#collapse').click(function() { content.find('> .container > .content').slideUp(); return false; });
	}
	
	link.click(function() { content.slideToggle(); return false; });
	$(tag).append(container);
	container.fadeIn();
	return content;
}

function collapseContainer(tag) {
	tag.slideToggle();
}

function addCodebox(tag, text) {
	var codebox = $('<pre class="codebox">'+$.trim(text)+'</pre>');
	codebox.css({'display':'none'});
	$(tag).append(codebox);
	codebox.fadeIn();
	return codebox;
}

function clearFloats(tag) {
	$(tag).append($('<div style="clear:both"></div>'));
}

