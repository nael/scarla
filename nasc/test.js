function generateSymbolTip(s) {
	var uniq = s.data('uniquename')
	var tip = ''
	
	tip += '<b>' + uniq + '</b><br/>'
	
	var sTy = s.data('type')
	if(sTy) {
		tip += '<b>Type</b> '
		tip += sTy
		tip += '<br/>'
	}
	
	var sTi = s.data('typeinfo')
	if(sTi) {
		tip += '<b>TypeInfo</b> '
	    tip += sTi
	    tip += '<br/>'
	}
	return tip
};

$(document).ready(function(){
	$(".symbol").each(function() {
		$(this).qtip({
			id: 'myTooltip',
			content: {
				text: '<span class=\'symbolInfo\'>' + generateSymbolTip($(this)) + '</span>'
			}
		});
	});
});