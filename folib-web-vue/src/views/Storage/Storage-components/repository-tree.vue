/**
功能：
作者：张佳宁
日期：
**/
<template>
    <div class="tree_container">
        <a-tree class="repositoryTree" 
            :replaceFields="replaceFields" :load-data="onLoadData" :tree-data="treeData" :show-line="true"
            :defaultExpandAll="false" @select="treeSelect" @expand="onExpand" show-icon default-expand-all>
            <a-icon slot="switcherIcon" type="down" />
            <a-icon slot="switcherIcon" type="folder-open" />
            <template slot="title" slot-scope="{ expanded,name,id,type,selected,fileType }">
                <div class="title_box">
                    <img v-if="fileType === 'document'" :src="getSrc(selected, type)" alt="" width="24">
                    <span v-if="fileType === 'document'" class="tree_title">
                        {{ id }}
                    </span>
                    <span v-else>
                        <a-icon class="tree_icon" v-if="type === 'dir'" :type="expanded ? 'folder-open' : 'folder'" />
                        <a-icon class="tree_icon" v-else :type="getIconType(name,type)"></a-icon>
                        <span class="tree_title">
                            {{ name }}
                        </span>
                    </span>
                </div>
            </template>
        </a-tree>
    </div>
</template>
<script>
import local from './images/local.svg'
import localCheck from './images/local-check.svg'
import remote from './images/remote.svg'
import remoteCheck from './images/remote-check.svg'
import virtual from './images/virtual.svg'
import virtualCheck from './images/virtual-check.svg'
import { getDockerArtifact, browse } from '@/api/folib'
import { name } from 'store/storages/cookieStorage'
export default {
    props: ['repositories'],
    data() {
        return {
            treeData: [],
            folibRepository:{},
            replaceFields:{
                key: 'key',
                title: 'name',
                children: 'children',
            }
        };
    },
    computed: {
        getSrc() {
            return (selected, type) => {
                let src = ''
                if (type === 'hosted') {
                    src = selected ? localCheck : local
                } else if (type === 'group') {
                    src = selected ? remoteCheck : remote
                } else {
                    src = selected ? virtualCheck : virtual
                }
                return src
            }
        },
        getIconType(){
            return (name,type) => {
                let icon = ''
                if (type === 'file') {
                    icon = 'file'
                }
                if(name.indexOf('.png') !== -1){
                    icon = 'file-image'
                }
                if(name.indexOf('.zip') !== -1){
                    icon = 'file-zip'
                }
                if(name.indexOf('.md') !== -1){
                    icon = 'file-markdown'
                }
                if(name.indexOf('.pdf') !== -1){
                    icon = 'file-pdf'
                }
                return icon
            }
        }
    },
    watch: {
        repositories: {
            handler(val) {
                if (val) {
                    this.treeData = JSON.parse(JSON.stringify(val))
                    this.treeData.map(ele => {
                        ele.fileType = 'document'
                        ele.key = ele.id
                        ele.name = ele.id
                        ele.artifactPath = ''
                    })
                    const e = {
                        node: {
                            dataRef: this.treeData[0]
                        }
                    }
                    this.treeSelect('',e)
                    console.log(this.treeData)
                }
            },
            immediate: true
        }
    },
    methods: {
        treeSelect(key, e) {
            if (e.node.dataRef.fileType == 'document') {
                this.folibRepository = e.node.dataRef
                if (this.folibRepository.status.indexOf('Out of Service') !== -1) {
                    this.$notification.warning({
                        message: this.$t('Store.ServiceShutdown')
                    })
                    return false
                }
                if (!this.folibRepository.allowsDirectoryBrowsing) {
                    this.$notification.warning({
                        message: this.$t('Store.BrowseNotEnabled')
                    })
                    return false
                }
                this.$emit('repositorySelect', e.node.dataRef)
            } else {
                this.$emit('treeSelect', key, e)
            }
        },
        onExpand() {
            this.$emit('onExpand')
        },
        onLoadData(treeNode) {
            if(treeNode.dataRef.fileType === 'document'){
                this.folibRepository = treeNode.dataRef
            }
            console.log(this.folibRepository)
            if (this.folibRepository.layout === 'Docker') {
                return new Promise(resolve => {
                    if (treeNode.dataRef.children) {
                        resolve()
                        return
                    }
                    getDockerArtifact(
                        this.folibRepository.storageId,
                        this.folibRepository.id,
                        treeNode.dataRef.artifactPath
                    ).then(res => {
                        treeNode.dataRef.children = []
                        if (res.directories.length > 0) {
                            const d = res.directories

                            d.forEach((item, index, d) => {
                                item.type = 'dir'
                                item.key = this.folibRepository.id + item.artifactPath
                                treeNode.dataRef.children.push(item)
                            })
                        }
                        if (res.files.length > 0) {
                            const a = res.files
                            a.forEach((item, index, a) => {
                                item.isLeaf = true
                                item.type = 'file'
                                item.key = this.folibRepository.id + item.artifactPath
                                treeNode.dataRef.children.push(item)
                            })
                        }
                        this.treeData = [...this.treeData]
                        resolve()
                    })
                })
            }
            return new Promise(resolve => {
                if (treeNode.dataRef.children) {
                    resolve()
                    return
                }
                browse(
                    this.folibRepository.storageId,
                    this.folibRepository.id,
                    treeNode.dataRef.artifactPath
                ).then(res => {
                    if (!treeNode.dataRef.children) {
                        treeNode.dataRef.children = []
                    }
                    if (res.directories.length > 0) {
                        const d = res.directories
                        d.forEach((item, index, d) => {
                            item.type = 'dir'
                            item.key = this.folibRepository.id + item.artifactPath
                        })
                        treeNode.dataRef.children = d
                    }
                    if (res.files.length > 0) {
                        const a = res.files
                        a.forEach((item, index, a) => {
                            item.isLeaf = true
                            item.type = 'file'
                            item.key = this.folibRepository.id + item.artifactPath
                        })
                        treeNode.dataRef.children = treeNode.dataRef.children.concat(a)
                    }

                    this.treeData = [...this.treeData]
                    resolve()
                })
            })
        },
        // getBrowse() {
        //     if (this.folibRepository.status.indexOf('Out of Service') !== -1) {
        //         this.$notification.warning({
        //             message: this.$t('Store.ServiceShutdown')
        //         })
        //         return false
        //     }
        //     if (!this.folibRepository.allowsDirectoryBrowsing) {
        //         this.$notification.warning({
        //             message: this.$t('Store.BrowseNotEnabled')
        //         })
        //         return false
        //     }
        //     browse(this.folibRepository.storageId, this.folibRepository.id, '')
        //         .then(res => {
        //             const d = res.directories
        //             d.forEach((item, index, d) => {
        //                 item.type = 'dir'
        //             })
        //             const f = res.files
        //             f.forEach((item, index) => {
        //                 item.isLeaf = true
        //                 item.type = 'file'
        //             })
        //             this.treeData = d.concat(f)
        //         })
        //         .catch(err => { })
        // },
    },
};
</script>
<style lang="scss" scoped>
.title_box {
    width: 100%;
    display: flex;
    align-items: center
}

.tree_title {
    margin-left: 5px;
    font-size: 16px;
    color: #67748e;
}
</style>
<style>
.repositoryTree .ant-tree-node-content-wrapper {
    width: 90% !important;
    height: 32px !important;
    line-height: 32px !important;
}

.repositoryTree .ant-tree-switcher_close,
.ant-tree-switcher_open {
    height: 32px !important;
    line-height: 32px !important;
}
.tree_container{
    height: 650px;
    overflow: auto;
}
.repositoryTree .ant-tree-switcher-noop{
    display: none !important;
}

.repositoryTree .tree_icon{
    font-size: 17px;
    font-weight: 500;
}
</style>