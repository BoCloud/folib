<template>

	<!-- Task card -->
	<a-card
		@click="$emit('edit', task.id, boardId)"
		:bordered="false" class="kanban-card mb-24"
	>

		<img v-if="task.cover" :src="task.cover" class="mb-15">

		<a-tag v-if="task.badge" class="mb-15" :class="[task.badge.class]">{{ task.badge.text }}</a-tag>

		<h6
			v-if="task.title"
			class="font-regular text-md mb-0"
			:class="[task.description || task.progress || task.attachments || task.members ? 'mb-10' : '']">{{ task.title }}</h6>

		<p v-if="task.description">{{ task.description }}</p>

		<a-progress v-if="task.progress" :percent="task.progress.value" :status="task.progress.status" :strokeColor="task.progress.color" :show-info="false" />

		<a-row v-if="task.attachments || task.members" type="flex" class="">
			<a-col>
				<div v-if="task.attachments">
					<a-icon type="paper-clip" class="m-0" /> {{ task.attachments }}
				</div>
			</a-col>
			<a-col class="ml-auto">
				<a-space v-if="task.members && task.members.length" :size="-8" class="avatar-chips">
					<a-avatar :size="24"
						v-for="avatar in task.members"
						:key="avatar"
						:src="avatar" />
				</a-space>
			</a-col>
		</a-row>
	</a-card>
	<!-- / Task card -->

</template>

<script>

	export default {
		props: {
			// Task object
			task: Object,
			
			// Task's board_id
			boardId: [String, Number],
		},
		data() {
			return {
			}
		},
	}

</script>

<style lang="scss">
</style>