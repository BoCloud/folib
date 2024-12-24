<template>

	<!-- Task card -->
	<a-card @click="$emit('edit', task.id, boardId)"
      :bordered="false" class="kanban-card mb-24"
	>
    <a-row :gutter="[24]">
      <a-col :span="4" class="pl-4">
        <a-avatar
            :size="48"
            shape="square"
            class="icon_sty"
        >
          <img :src="'images/folib/'+getLayoutType(task)+'.svg'" style="width: 100%;" alt="">
        </a-avatar>
      </a-col >
      <a-col :span="15">
        <div>
          <a-tag class="mb-5 bg-warning">{{$t('Kanban.Storage')}}</a-tag>
          <span>{{ task.storageId }}</span>
        </div>
        <div>
          <a-tag class="mb-5 bg-success">{{$t('Kanban.Repository')}}</a-tag>
          <span>{{ task.repositoryId }}</span>
          <a-icon v-if="task.scope===2" class="ml-10" :style="{color:'#52C41A'}" type="unlock" />
        </div>
        <div>
          <a-tag class="mb-0 bg-gradient-secondary">{{$t('Kanban.Type')}}</a-tag>
          <span>{{ task.type }}</span>
          <a-icon v-if="task.scope===2" class="ml-10"  type="unlock" />
        </div>
      </a-col>
      <a-col :span="3" class="pl-0 actions-col">
        <a-tag v-if="task.isDefault&&boardId==='folibGoup'" color="blue" >{{$t('Kanban.DefaultRepository')}}</a-tag>
        <a-button v-if="!task.isDefault&&task.type==='hosted'&&boardId==='folibGoup'" type="link" class="p-0"
        @click="setDefault">{{$t('Kanban.SetDefaultRepository')}}</a-button>
        <a-button v-if="task.isDefault&&task.type==='hosted'&&boardId==='folibGoup'" type="link" class="p-0 mt-8"
        @click="cancelDefault">{{$t('Kanban.CancelDefaultRepository')}}</a-button>
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
      },
      setDefault(){
        this.$emit('setDefault',this.task.id,this.boardId);
      },
      cancelDefault(){
        this.$emit('cancelDefault',this.task.id,this.boardId);
      }
    }
	}

</script>

<style lang="scss" scoped>

.icon_sty {
  border-radius: 8px; 
  background-image: linear-gradient(310deg, rgb(250, 251, 252), rgb(221, 221, 221));
  box-shadow: 0px 0px 6px 2px rgba(0, 0, 0, 0.1);
}
</style>
