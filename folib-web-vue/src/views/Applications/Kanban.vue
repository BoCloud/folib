<!-- 
	This is the Kanban page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

	<div class="kanban-page mb-24">

		<!-- Kanban App component -->
		<KanbanApp></KanbanApp>
		<!-- / Kanban App component -->

	</div>

</template>

<script>

	import KanbanApp from "../../components/Kanban/Kanban" ;

	export default {
		components: {
			KanbanApp,
		},
		data() {
			return {
			}
		},
		methods: {
		},
	}

</script>

<style lang="scss">
</style>