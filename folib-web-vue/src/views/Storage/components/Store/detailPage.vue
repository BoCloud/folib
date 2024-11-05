/**
功能：新增详情页
作者：张佳宁
日期：
**/
<template>
    <div class=''>
        <a-descriptions
            :column="1"
            style="word-break: break-all;word-wrap: break-word;"
          >
            <a-descriptions-item :label="$t('Store.OwningSpace')">
              {{ currentTreeNode.storageId }}
            </a-descriptions-item>
            <a-descriptions-item v-if="!currentTreeNode.layout" :label="$t('Store.OwnedWarehouse')">
              {{ currentTreeNode.repositoryId }}
            </a-descriptions-item>
            <a-descriptions-item v-if="currentTreeNode.layout" :label="$t('Store.RepositoryType')">
              {{ currentTreeNode.layout }}
            </a-descriptions-item>
            <a-descriptions-item v-if="currentTreeNode.layout" :label="$t('Store.StrategyType')">
              {{ currentTreeNode.type }}
            </a-descriptions-item>
            <a-descriptions-item :label="$t('Store.Name')">
              {{ currentTreeNode.name }}
            </a-descriptions-item>
            <a-descriptions-item v-if="currentTreeNode.layout" :label="$t('Store.ThePath')">
              <span>{{ currentTreeNode.name }}</span>
            </a-descriptions-item>
            <a-descriptions-item v-if="!currentTreeNode.layout" :label="$t('Store.ThePath')">
              {{ currentTreeNode.artifactPath }}
            </a-descriptions-item>
            <a-descriptions-item v-if="currentTreeNode.layout" :label="$t('Store.ArtifactMaxSize')">
              {{ fileSizeConver(currentTreeNode.artifactMaxSize)}}
            </a-descriptions-item>
            <a-descriptions-item v-if="currentTreeNode.remoteRepository" :label="$t('Store.ProxyAddress')">
               <p class="copy-p">{{ currentTreeNode.remoteRepository.url }}</p>
            </a-descriptions-item>
                <a-descriptions-item class="group-descriptions" :label="$t('Store.GroupRepositories')"
                        v-if="currentTreeNode.groupRepositories && currentTreeNode.groupRepositories.length > 0">
                        <div class="group-repositories-container">
                          <a-tooltip>
                            <template slot="title">
                              <div v-for="(repo, index) in currentTreeNode.groupRepositories" :key="index">{{ repo }}
                              </div>
                            </template>
                            <p class="ellipsis-text"> {{ currentTreeNode.groupRepositories.join('\n') }}</p>
                        </a-tooltip>
                  </div>
            </a-descriptions-item>
            <!-- <a-descriptions-item :label="$t('Store.ModifyTheTime')">
              {{ formateDate(currentTreeNode.lastModified) }}
            </a-descriptions-item> -->
          </a-descriptions>
    </div>
</template>

<script>
import { fileSizeConver, formateDate } from "@/utils/layoutUtil";
export default {
  props: ['folibRepository','isStoreView'],
    data() {
        return {
            

        }
    },
    computed:{
        currentTreeNode() {
          if(this.isStoreView){
            let currentTreeNode={...this.folibRepository};
            currentTreeNode.name=this.folibRepository.id;
            return currentTreeNode;
          }else{
            return this.$store.state.currentTreeNode
          }
        }
    },
    mounted() {
      
    },
    methods:{
        fileSizeConver(size){
            if(size){
                return fileSizeConver(size)
            }
            return "--"
        }
    }
}
</script>