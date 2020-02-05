
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
  a=s.createElement(o), m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-154687382-1', 'auto');
ga('send', 'pageview', {
  'page': '/CCB',
  'title': 'HomePage'
});


#------------


  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
  gtag('config', 'UA-154687382-1');


#--------------



from John:

$(document).on('click', element name (i.e. named element) that the clicked on....



  
$(document).on('shiny:inputchanged', function(event) {
       if (event.name == 'exposure' || event.name == 'sensitivity') {
          gtag('event', event.name, {
              'event_action': 'Select Input',
              'event_category': 'Vulnerability',
              'event_label': event.value
          });
      
       }
       if (event.name == 'cnty1') {
          gtag('event', event.name, {
              'event_action': 'Select Input',
              'event_category': 'County Snapshot',
              'event_label': event.value
          });
       }
      if (event.name == 'cnty' || event.name == 'ind' ) {
          gtag('event', event.name, {
              'event_action': 'Select Input',
              'event_category': 'Single Indicator',
              'event_label': event.value
          });
       }
      if (event.name == 'cntyDNLD' || event.name == 'indDNLD' ) {
          gtag('event', event.name, {
              'event_action': 'Select Input',
              'event_category': 'Query the Data',
              'event_label': event.value
          });
       }
     });
     
        
