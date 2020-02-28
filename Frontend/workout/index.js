Vue.component('routine', {
    props: ['routine'],
    template: `
    <div class="routine">
      <h3 class="routine_name">{{ routine.name }}</h3>
      <div class="ui button">
      Record
      </div>
    </div>
    `
})

var vm = new Vue({
    el: '#app',
    data: {
        routines: [
            { id: 1, name: 'test' },
        ]
    }
})
