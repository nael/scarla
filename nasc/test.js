function generateSymbolTip(s) {
	var uniq = s.data('uniquename')
	var sTy = s.data('type')
	var tip = ''
	
	tip += '<b>' + uniq + '</b><br/>'
	tip += '<b>Type</b> '
	if(sTy) tip += sTy
	else tip += "not typed"
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