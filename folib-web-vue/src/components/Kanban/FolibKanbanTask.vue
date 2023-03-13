<template>

	<!-- Task card -->
	<a-card @click="$emit('edit', task.id, boardId)"
      :bordered="false" class="kanban-card mb-24"
	>
    <a-row :gutter="[24]">
      <a-col :span="6">
        <a-avatar
            :size="32"
            shape="square"
            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
        >
          <img :src="'images/folib/'+getLayoutType(task)+'.svg'" style="width: 100%;" alt="">
        </a-avatar>
      </a-col >
      <a-col :span="18">
        <div>
          <a-tag class="mb-5 bg-warning">存储空间</a-tag>
          <span>{{ task.storageId }}</span>
        </div>
        <div>
          <a-tag class="mb-0 bg-success">仓库名称</a-tag>
          <span>{{ task.repositoryId }}</span>
          <a-icon v-if="task.scope===2" class="ml-10" :style="{color:'#52C41A'}" type="unlock" />
        </div>
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
