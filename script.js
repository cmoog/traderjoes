let data = [], chart = null;

$(document).ready(() => {
  let dateFormatter = t => {
    let d = t.split(" ")[0], m = parseInt(d.split("-")[1]) * 3 - 1;
    return `${"--JanFebMarAprMayJunJulAugSepOctNovDec".substring(m, m + 3)} ${d.split("-")[2]}, ${d.split("-")[0]}`
  }
  let storeHandler = _ => {
    let storeId = $('#store-select').select2('data')[0].id;
    $('.price-changes').remove();
    $('.current-prices').remove();
    fetch(`./prices-${storeId}.json`)
      .then(r => { return r.json(); })
      .then(r => {
        data = r;
        for (let i = 0; i < r.length; i++) {
          let d = r[i];
          if (d.pbefore_date != null && d.pafter_date != null) {
            $('<tr>').addClass('price-changes').append(
              $('<td>').text(dateFormatter(d.pafter_date)),
              $('<td>').append(
                $('<a>')
                  .attr('href', `https://traderjoes.com/home/products/pdp/${d.psku}`)
                  .attr('target', '_blank')
                  .text(d.pitem_title)
              ),
              $('<td>').text(d.pbefore_price),
              $('<td>')
                .addClass(d.pbefore_price > d.pafter_price ? 'green' : 'red')
                .text(d.pafter_price)
            ).appendTo('#price-changes');
          }
          else if (d.pafter_date == null) {
            $('<tr>').addClass('current-prices').append(
              $('<td>').append(
                $('<a>')
                  .attr('href', `https://traderjoes.com/home/products/pdp/${d.psku}`)
                  .attr('target', '_blank')
                  .text(d.pitem_title)
              ),
              $('<td>').text(d.pbefore_price)
            ).appendTo('#current-prices');
          }
        }
        itemHandler();
      });
  };
  let itemHandler = _ => {
    let items = $('#item-select').select2('data').map(d => ({sku: d.id, name: d.text}));
    if (items.length == 0) return;
    let ids = items.map(d => d.sku),
      subset = data.filter(d => ids.indexOf(d.psku) > -1),
      datasets = items.map(d => ({
        label: d.name,
        data: subset.filter(p => p.psku == d.sku && p.pafter_date != null).map(p => ({x: p.pafter_date, y: p.pafter_price}))
      }));
    console.log(datasets)
    if (chart != null) chart.destroy();
    chart = new Chart(
      document.getElementById('chart'),
      {
        type: 'line',
        data: {
          datasets
        },
        options: {
          responsive: true,
          plugins: {
            legend: {
              position: 'top',
            }
          },
          scales: {
            x: {
              type: 'time',
              time: {
                unit: 'day'
              },
            },
            y: {
              title: {
                display: true,
                text: 'Price'
              }
            }
          }
        },
      }
    );
  };
  $('.dropdown').select2({ width: '70%' });
  $('#store-select').on('select2:select', storeHandler);
  $('#item-select').on('select2:select', itemHandler);
  $('#item-select').on('select2:unselect', itemHandler);
  storeHandler();
});