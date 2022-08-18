<template>

	<!-- Task card -->
	<a-card @click="$emit('edit', task.id, boardId)"
      :bordered="false" class="kanban-card mb-24"
	>
    <a-row :gutter="[24]">
      <a-col :span="8">
        <a-avatar
            :size="32"
            shape="square"
            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
        >
          <img :src="'images/folib/'+getLayoutType(task)+'.svg'" style="width: 100%;" alt="">
        </a-avatar>
      </a-col >
      <a-col :span="16">
        <h6 class="font-regular text-md mb-0">{{ task.id }}</h6>
      </a-col>
    </a-row>


	</a-card>
	<!-- / Task card -->

</template>

<script>
import {getLayoutType }from "@/utils/layoutUtil"
	export default {
    components: {
    },
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
    methods: {
      getLayoutType(item){
        return  getLayoutType(item)
      }
    }
	}

</script>

<style lang="scss">
</style>
