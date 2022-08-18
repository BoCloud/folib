<template>

	<div>

		<!-- Kanban head - New kanban button -->
		<div class="kanban-head my-15">

			<div class="ml-auto">
				<h6 class="text-sm text-muted">Team members :</h6>
				<a-space :size="-12" class="avatar-chips">
					<a-avatar :size="36" src="images/face-1.jpg" />
					<a-avatar :size="36" src="images/face-4.jpg" />
					<a-avatar :size="36" src="images/face-2.jpg" />
					<a-avatar :size="36" src="images/face-3.jpg" />
					<a-avatar :size="36" src="images/face-5.jpeg" />
				</a-space>
			</div>
			<div>
				<a-button type="primary" @click="showBoardFormModal = true">
					<a-icon type="plus" class="m-0" />
				</a-button>
			</div>

		</div>
		<!-- / Kanban head - New kanban button -->

		<!-- Kanban body - Boards and tasks -->
		<div id="kanban" class="kanban">
			
			<!-- Draggable component comes from vuedraggable. It provides drag & drop functionality -->
			<draggable :list="boards" :animation="200" class="kanban-boards" ghost-class="ghost-card" group="boards">
				<!-- Each element from here will be draggable and animated. -->

				<!-- Kanban Board -->
				<KanbanBoard
					v-for="(board) in boards"
					:key="board.id"
					:board="board"
					@newKanbanAdded="addNewTask"
				>
				
					<!-- Draggable component from vuedraggable. -->
					<draggable :list="board.tasks" :animation="200" ghost-class="ghost-card" group="tasks">
						
						<!-- Kanban Task -->
						<KanbanTask
							v-for="(task) in board.tasks"
							:key="task.id"
							:task="task"
							:boardId="board.id"
							@edit="showEditModal"
						></KanbanTask>
						<!-- / Kanban Task -->

					</draggable>
					<!-- / Draggable component from vuedraggable. -->
				
				</KanbanBoard>
				<!-- / Kanban Board -->

			</draggable>
			<!-- / Draggable component from vuedraggable. -->

			<!-- Edit task form modal -->
			<a-modal v-model="showTaskFormModal" :footer="null" :forceRender="true" title="Title" on-ok="showTaskFormModal = false">
				<a-form
					:form="updateTaskForm"
					:hideRequiredMark="true"
					@submit.prevent="handleTaskUpdateSubmit"
				>
					<a-row :gutter="[24]">
						<a-col :span="24">
							
							<a-form-item class="mb-10" :colon="false">
								<a-input
									v-decorator="[ 'title', ]"
									placeholder="Task title">
									<a-icon slot="prefix" type="edit" />
								</a-input>
							</a-form-item>
							<a-form-item class="mb-10" :colon="false">
								<a-input
									v-decorator="[ 'assignee', ]"
									placeholder="Task assignee">
									<a-icon slot="prefix" type="user" />
								</a-input>
							</a-form-item>
							<a-form-item class="mb-10" :colon="false">
								<a-textarea
									v-decorator="[ 'description', ]"
									placeholder="Task Description"
									:rows="4" />
							</a-form-item>
						</a-col>
						<a-col :span="12">
							<a-button key="back" @click="deleteCurrentTask" class="px-30" size="small" type="danger">Delete</a-button>
						</a-col>
						<a-col :span="12" class="text-right">
							<a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">Update</a-button>
							<a-button key="back" @click="showTaskFormModal = false" class="px-30 ml-10" size="small">Cancel</a-button>
						</a-col>
					</a-row>
				</a-form>
			</a-modal>
			<!-- / Edit task form modal -->
			
			<!-- New board form modal -->
			<a-modal v-model="showBoardFormModal" :footer="null" :forceRender="true" title="Title" on-ok="showBoardFormModal = false">
				<a-form
					:form="newBoardForm"
					:hideRequiredMark="true"
					@submit.prevent="handleAddNewBoardSubmit"
				>
					<a-row :gutter="[24]">
						<a-col :span="24">
							
							<a-form-item class="mb-10" :colon="false">
								<a-input
									v-decorator="[
										'title',
										{ rules: [{ required: true, message: 'Please fill task title' }] },
									]"
									placeholder="Board title">
									<a-icon slot="prefix" type="edit" />
								</a-input>
							</a-form-item>
						</a-col>
						<a-col :span="12">
						</a-col>
						<a-col :span="12" class="text-right">
							<a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">Add</a-button>
							<a-button key="back" @click="showBoardFormModal = false" class="px-30 ml-10" size="small">Cancel</a-button>
						</a-col>
					</a-row>
				</a-form>
			</a-modal>
			<!-- / New board form modal -->

		</div>
		<!-- / Kanban body - Boards and tasks -->

	</div>

</template>

<script>

	import draggable from "vuedraggable";
	import KanbanBoard from "./KanbanBoard";
	import KanbanTask from "./KanbanTask";

	export default {
		props: {
			items: Array,
		},
		components: {
			draggable,
			KanbanBoard,
			KanbanTask,
		},
		data() {
			return {

				// Form object to update tasks.
      			updateTaskForm: this.$form.createForm(this, { name: 'update_task' }),

				// Form object to create boards.
      			newBoardForm: this.$form.createForm(this, { name: 'new_board' }),
				  
				// Whether to show edit task modal.
      			showTaskFormModal: false,
				  
				// Whether to show new board modal.
      			showBoardFormModal: false,
				  
				// board_id of the task being edited.
      			editableBoardId: null,
				  
				// Id of the task being edited.
      			editableTaskId: null,
				  
				// Kanban data.
				boards: [
					{
						id: "_backlog",
						title: "Backlog",
						tasks: [
							{
								id: "_task_1_title_id",
								title: "Click me to change title",
								description: '',
							},
							{
								id: "_task_2_title_id",
								title: "Drag me to 'In progress' section",
								description: '',
							},
							{
								id: "_task_do_something_id",
								title: "",
								cover: 'images/office-dark.jpg',
								badge: {
									class: 'bg-danger text-white',
									text: 'PENDING',
								},
								description: 'Website Design: New cards for blog section and profile details',
								attachments: 3,
								members: [
									'images/team-1.jpg',
									'images/team-2.jpg',
									'images/team-3.jpg',
								],
							},
						]
					},
					{
						id: "_progress",
						title: "In progress",
						tasks: [
							{
								id: "_task_3_title_id",
								title: "",
								badge: {
									class: 'bg-warning',
									text: 'ERRORS',
								},
								description: 'Fix Firefox errors',
								attachments: 11,
								members: [
									'images/team-3.jpg',
									'images/team-2.jpg',
								],
								assignee: 'Done Joe',
							},
							{
								id: "_task_4_title_id",
								title: "",
								badge: {
									class: 'bg-primary text-white',
									text: 'UPDATES',
								},
								description: 'Argon Dashboard PRO - Angular 11',
								attachments: 3,
								members: [
									'images/team-5.jpg',
									'images/team-4.jpg',
								],
								assignee: 'Done Joe',
							},
							{
								id: "_task_do_something_4_id",
								title: "",
								cover: "images/meeting.jpg",
								badge: {
									class: 'bg-primary text-white',
									text: 'UPDATES',
								},
								description: 'Vue 3 Updates',
								attachments: 9,
								members: [
									'images/team-1.jpg',
									'images/team-2.jpg',
									'images/team-4.jpg',
								],
								assignee: 'Done Joe',
							},
						]
					},
					{
						id: "_working",
						title: "In review",
						tasks: [
							{
								id: "_task_do_something_2_id",
								title: "",
								badge: {
									class: 'bg-warning',
									text: 'IN TESTING',
								},
								attachments: 11,
								description: "Responsive Changes",
								members: [
									'images/team-3.jpg',
									'images/team-2.jpg',
								],
								assignee: 'Done Joe',
							},
							{
								id: "_task_run_id",
								title: "",
								badge: {
									class: 'bg-success text-white',
									text: 'IN REVIEW',
								},
								description: "Change images dimension",
								progress: {
									color: '#52C41A',
									value: 80,
								},
								members: [
									'images/team-3.jpg',
								],
								assignee: 'Done Joe',
							},
							{
								id: "_task_do_something_3_id",
								title: "",
								badge: {
									class: 'bg-success text-white',
									text: 'IN REVIEW',
								},
								description: "Update Links",
								progress: {
									color: '#1890FF',
									value: 60,
								},
								attachments: 6,
								members: [
									'images/team-5.jpg',
									'images/team-1.jpg',
								],
								assignee: 'Done Joe',
							}
						]
					},
					{
						id: "_done",
						title: "Done",
						tasks: [
							{
								id: "_task_all_right_id",
								title: "",
								cover: 'images/home-decor-1.jpg',
								badge: {
									class: 'bg-success text-white',
									text: 'DONE',
								},
								description: "Redesign for the home page",
								attachments: 8,
								members: [
									'images/team-5.jpg',
									'images/team-1.jpg',
									'images/team-4.jpg',
								],
								assignee: 'Done Joe',
							},
							{
								id: "_task_ok_id",
								title: "",
								badge: {
									class: 'bg-success text-white',
									text: 'DONE',
								},
								description: 'Schedule winter campaign',
								attachments: 2,
								members: [
									'images/team-1.jpg',
									'images/team-4.jpg',
								],
								assignee: 'Done Joe',
							},
						]
					}
				],
			}
		},
		methods: {

			// Add new task to a board.
			addNewTask( board_id, newTaskFields ) {
				for( let i = 0 ; i < this.boards.length ; i++ ) {
					const board = this.boards[ i ] ;

					if( board.id == board_id ) {
						board.tasks.push( newTaskFields ) ;
					}
				}
			},

			// update a task.
			updateTask( board_id, task_id, updatedTaskFields ) {
				let board = this.boards.filter( (board) => {
					return board.id == board_id ;
				})[0] ;

				let task = board.tasks.filter( (task) => {
					return task.id == task_id ;
				})[0] ;
				
				for( const key in updatedTaskFields ) {
					task[ key ] = updatedTaskFields[ key ] ;
				}
				
			},

			// Place task information on the form for editing and show the modal.
			showEditModal( task_id, board_id ) {

				this.editableBoardId = board_id ;
				this.editableTaskId = task_id ;

				let board = this.boards.filter( (board) => {
					return board.id == board_id ;
				})[0] ;

				let task = board.tasks.filter( (task) => {
					return task.id == task_id ;
				})[0] ;

      			this.showTaskFormModal = true;

				this.updateTaskForm.setFieldsValue({
					title: task.title,
					assignee: task.assignee,
					description: task.description,
				}) ;
			},

			// Event handler for the task-edit form submission.
			handleTaskUpdateSubmit(e) {
				this.updateTaskForm.validateFields( (err, values) => {
					if ( !err ) {
						this.updateTask( this.editableBoardId, this.editableTaskId, values )
						
						this.showTaskFormModal = false;
						this.updateTaskForm.resetFields() ;
					}
				});
			},

			// Event handler for the task-delete button click.
			deleteCurrentTask(e) {
				let board = this.boards.filter( (board) => {
					return board.id == this.editableBoardId ;
				})[0] ;

				board.tasks = board.tasks.filter( (task) => task.id != this.editableTaskId ) ;
				
				this.showTaskFormModal = false;
				this.updateTaskForm.resetFields() ;
			},

			// Event handler for the new-board form submission.
			handleAddNewBoardSubmit(e) {
				this.newBoardForm.validateFields( (err, values) => {
					if ( !err ) {
						this.addNewBoard( values )
						
						this.showBoardFormModal = false;
						this.newBoardForm.resetFields() ;
					}
				});
			},

			// Add new board to a kanban data.
			addNewBoard( newBoardFields ) {
				this.boards.push( {
					id: newBoardFields["title"].replace(' ', '-'),
					title: newBoardFields["title"],
					tasks: [],
				} ) ;
			},
			
		},
	}

</script>

<style lang="scss">
	.ghost-card {
		opacity: 0.5;
		background: #F7FAFC;
		border: 1px solid #4299e1;
	}
</style>