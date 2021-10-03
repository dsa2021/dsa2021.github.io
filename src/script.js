var request = new XMLHttpRequest();
var data_url = 'data/data.json';

request.open('GET', data_url);

$(document).ready(function(){
  $("#button").click(function () { 
      var url = $('#url').val();
      $("#frame").attr("src", url);
  });
});

function build_html(hj, message,deaths, state, city, confirmed, confirmed_diff, date){
    var text;
    text =  '<b>Execução instaurada hoje: </b>'+hj+'.';
    text += '<br>'
    text += (message != 'VAZIO')?('<b>Resultado de erro: </b>' + message +'.') :'';
    text += '<br>'
    text += '<br>'
    if (date != 'VAZIO'){
        text += '<b>[Atenção] Dados coletados abaixo.</b>'
        text += '<br>'
        text += '<b>Cidade:</b> '+ String(city) +'-'+String(state);
        text += '<br>'
        text += '<b>Óbitos:</b> '+ String(deaths);
        text += '<br>'
        text += `<b>Casos confirmados: </b>${confirmed} (+${confirmed_diff} foram últimos registrados.)`;
        text += '<br>'
        text += '<b>Data de atualização: </b>' + String(date);
    }
    else{ text += '<b>[Problema]Não foram coletados dados hoje!</b>'; }
    return text;
}

request.onload = function(){
    // if (this.status == 200){
        let z = 'VAZIO'
        let x = String(this.response).trim();
        let data = JSON.parse(x);
        let dayName = new Array ("domingo", "segunda", "terça", "quarta", "quinta", "sexta", "sábado")
        let monName = new Array ("janeiro", "fevereiro", "março", "abril", "maio", "junho", "agosto", "outubro", "novembro", "dezembro")
        let now = new Date
    
        let hj = dayName[now.getDay() ]+ ","+ now.getDate()+ " de "+monName [now.getMonth()]+  " de "  + now.getFullYear ();
        let message = ('message' in data)? data['message']:z;
        let deaths = ('results' in data && 'deaths' in data['results'][0])? data['results'][0]['deaths']: z;
        let state = ('results' in data && 'state' in data['results'][0])? data['results'][0]['state']: z;
        
        let city = ('results' in data && 'city' in data['results'][0])? data['results'][0]['city']: z;
        let confirmed_today = ('results' in data && 'confirmed' in data['results'][0])? data['results'][0]['confirmed']:z;
        let confirmed_yesterday = ('results' in data && 'confirmed' in data['results'][1])? data['results'][1]['confirmed']:z;
        let confirmed_diff = parseInt(confirmed_today) - parseInt(confirmed_yesterday);
        let update_date = ('results' in data && 'date' in data['results'][0])? data['results'][0]['date']:z;
        // let html_text = build_html(hj, message, deaths, state, city, confirmed_today, confirmed_diff, update_date);
        let html_text = '<h1> Links de Análise </h1>'
        
<<<<<<< HEAD
        document.getElementById("status").innerHTML= html_text;
        //document.body.innerHTML = html_text;
        //document.body.innerHTML = html_text;
=======
        html_text = html_text + '<p><a href="https://htmlpreview.github.io/?https://github.com/dsa2021/dsa2021.github.io/blob/main/analise/dataprep.html">Análise via Dataprep</a></p>'
        html_text = html_text + '<p><a href="https://htmlpreview.github.io/?https://github.com/dsa2021/dsa2021.github.io/blob/main/analise/pandasprofiling.html">Análise via Pandas Profiling</a></p>'    
        html_text = html_text + '<p><a href="https://htmlpreview.github.io/?https://github.com/dsa2021/dsa2021.github.io/blob/main/analise/sweetviz.html">Análise via SweetViz</a></p>'
        html_text = html_text + '<p><a href="https://htmlpreview.github.io/?https://github.com/dsa2021/dsa2021.github.io/blob/main/analise/AutoViz_Plots/riscofogo/autoviz.html">Análise via Autoviz</a></p>'
    
        document.body.innerHTML = html_text;
>>>>>>> 23042cda88e5cde72435e025d7f5a12943e95e50
        //document.body.innerHTML = city;
   // }
   // else{ document.body.innerHTML = 'Sem dado!'; }
}

request.onerror = function(){

}

request.send();
