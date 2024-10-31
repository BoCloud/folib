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
            <a-descriptions-item v-if="currentTreeNode.layout" :label="$t('Store.FileSize')">
              {{ fileSizeConver(currentTreeNode.artifactMaxSize)}}
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
    data() {
        return {

        }
    },
    computed:{
        currentTreeNode() {
            return this.$store.state.currentTreeNode
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