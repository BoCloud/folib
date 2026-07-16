<template>
			
	<!-- Board card -->
	<a-card
		:bordered="false"
		class="kanban-board header-solid mb-24"
	>
		<h6  slot="title" class="font-semibold m-0">{{ board.title }}</h6>
<!--		<a-button slot="extra" @click="showForm = true" size="small" class="px-30">-->
<!--			<a-icon type="plus" class="m-0" />-->
<!--		</a-button>-->
		<div class="kanban-board-content">

			<slot></slot>

		</div>
	</a-card>
	<!-- Board card -->

</template>

<script>

	export default {
		props: {
			board: Object,
		},
		data() {
			return {

				// Form object for creating new task.
      			form: this.$form.createForm(this, { name: 'task_form' }),

				// Whether or not to show the new-task form.
				showForm: false,

			}
		},
		methods: {

			// Event handler for the new-task form submission.
			handleSubmit() {

				this.form.validateFields( (err, values) => {
					if ( !err ) {
						let newTaskDescription = this.form.getFieldValue('newTaskDescription') ;

						this.$emit('newKanbanAdded', this.board.id, {
							id: newTaskDescription.replace(' ', '-') + Math.floor(Math.random() * 100),
							title: newTaskDescription,
							description: '',
						}) ;

						this.form.setFieldsValue({newTaskDescription: ''}) ;
						this.showForm = false ;
					}
				});
			},
			
		},
	}

</script>

<style lang="scss" scoped>
</style>
