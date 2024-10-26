/**
功能：
作者：张佳宁
日期：
**/
<template>
    <div class="tree_container" :key="key" @scroll="handleScroll">
        <a-tree class="repositoryTree" 
            :replaceFields="replaceFields" 
            :load-data="onLoadData" 
            :tree-data="treeData" 
            :show-line="true"
            :defaultExpandAll="false" 
            @select="treeSelect" 
            @expand="onExpand" 
            show-icon 
            default-expand-all
        >
            <a-icon slot="switcherIcon" type="down" />
            <a-icon slot="switcherIcon" type="folder-open" />
            <template slot="title" slot-scope="{ expanded,name,id,type,selected,fileType }">
                <div class="title_box">
                    <img v-if="fileType === 'document'" :src="getSrc(selected, type)" alt="" width="24">
                    <span v-if="fileType === 'document'" class="tree_title">
                        {{ id }}
                    </span>
                    <span v-else>
                        <a-icon class="tree_icon" v-if="type === 'dir' || type === 'DIR'" :type="expanded ? 'folder-open' : 'folder'" />
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
import { getDockerArtifact, browse, getArtifact } from '@/api/folib'
import { getLayoutType } from '@/utils/layoutUtil'
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
            },
            key: 0,
            repositoryType:'',
            artifactPath:''
        };
    },
    computed: {
        getSrc() {
            return (selected, type) => {
                let src = ''
                if (type === 'hosted') {
                    src = selected ? localCheck : local
                } else if (type === 'group') {
                    src = selected ? virtualCheck : virtual
                } else {
                    src = selected ? remoteCheck : remote
                }
                return src
            }
        },
        getIconType(){
            return (name,type) => {
                const _name = name.toLowerCase()
                const _type = type.toLowerCase()
                let icon = ''
                if (_type === 'file') {
                    icon = 'file'
                }
                if(_name.indexOf('.png') !== -1){
                    icon = 'file-image'
                }
                if(_name.indexOf('.zip') !== -1){
                    icon = 'file-zip'
                }
                if(_name.indexOf('.md') !== -1){
                    icon = 'file-markdown'
                }
                if(_name.indexOf('.pdf') !== -1){
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
                        ele.newDetailPage = true
                    })
                    this.key ++
                    const e = {
                        node: {
                            dataRef: this.treeData[0]
                        }
                    }
                    this.treeSelect('',e)
                }
            },
            immediate: true
        }
    },
    methods: {
        handleScroll(event){
            const { scrollTop, clientHeight, scrollHeight } = event.target;
            // 当滚动到底部时加载更多
            if (scrollTop + clientHeight >= scrollHeight) {
                this.$emit('loadMore')
            }
        },
        // 判断那些文件类型是可以打开的
        getFileIsOpen(name){
            const _name = name.toLowerCase()
            const tarArr = ['.tar','.jar','.zip','.7z','.tar.gz']
            let key = false
            tarArr.forEach(ele => {
                if(_name.indexOf(ele) !== -1){
                    key = true
                }
            })
            return key
        },
        treeSelect(key, e) {
            console.log(key,e,'eeeeeeeeeee')
            const {newDetailPage} = e.node.dataRef
            this.$store.commit('setNewDetailPage', !!newDetailPage)
            if (e.node.dataRef.fileType == 'document') {
                this.folibRepository = e.node.dataRef
                this.repositoryType = getLayoutType(this.folibRepository)
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
            }else{
                this.$emit('treeSelect', key, e)
            }
            if(!!newDetailPage){
                console.log(e.node.dataRef)
                const {id, storageId} = this.folibRepository
                let params = e.node.dataRef
                params.repositoryId = id
                params.storageId = storageId
                this.$store.commit('setCurrentTreeNode', params)
            }
        },
        onExpand() {
            this.$emit('onExpand')
        },
        onLoadData(treeNode) {
            console.log(treeNode,'treeNodetreeNodetreeNode')
            if(treeNode.dataRef.fileType === 'document'){
                this.folibRepository = treeNode.dataRef
                this.repositoryType = getLayoutType(this.folibRepository)
            }
            const {storageId,id,layout} = this.folibRepository
            const {artifactPath,name} = treeNode.dataRef
            const params = {
                treeNode,
                storageId,
                id,
                layout,
                artifactPath,
                name
            }
            if(this.getFileIsOpen(name)){
                return this.getPackagePreview(params)
            }
            
            if (layout === 'Docker') {
                return new Promise(resolve => {
                    if (treeNode.dataRef.children) {
                        resolve()
                        return
                    }
                    getDockerArtifact(
                        storageId,
                        id,
                        artifactPath
                    ).then(res => {
                        treeNode.dataRef.children = []
                        if (res.directories.length > 0) {
                            const d = res.directories
                            d.forEach((item, index, d) => {
                                item.type = 'dir'
                                item.key = id + item.artifactPath
                                treeNode.dataRef.children.push(item)
                            })
                        }
                        if (res.files.length > 0) {
                            const a = res.files
                            a.forEach((item, index, a) => {
                                item.isLeaf = !this.getFileIsOpen(item.name)
                                item.type = 'file'
                                item.key = id + item.artifactPath
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
                    storageId,
                    id,
                    artifactPath
                ).then(res => {
                    if (!treeNode.dataRef.children) {
                        treeNode.dataRef.children = []
                    }
                    if (res.directories.length > 0) {
                        const d = res.directories
                        d.forEach((item, index, d) => {
                            item.type = 'dir'
                            item.key = id + item.artifactPath
                        })
                        treeNode.dataRef.children = d
                    }
                    if (res.files.length > 0) {
                        const a = res.files
                        a.forEach((item, index, a) => {
                            item.isLeaf = !this.getFileIsOpen(item.name)
                            item.type = 'file'
                            item.key = id + item.artifactPath
                        })
                        treeNode.dataRef.children = treeNode.dataRef.children.concat(a)
                    }

                    this.treeData = [...this.treeData]
                    resolve()
                })
            })
        },
        // 获取可以继续打开的文件的目录（对应包预览）
        getPackagePreview({treeNode,storageId,id,artifactPath}){
            return new Promise(resolve => {
                if (treeNode.dataRef.children) {
                    resolve()
                    return
                }
                getArtifact(
                    this.repositoryType,
                    storageId,
                    id,
                    artifactPath
                ).then(res => {
                    function setNewDetailPage(arr){
                        arr.forEach(ele => {
                            ele.newDetailPage = true
                            ele.artifactPath = `${id}/${artifactPath}/${ele.name}`
                            if(ele?.children?.length){
                                setNewDetailPage(ele.children)
                            }
                        })
                    }
                    treeNode.dataRef.children = []  
                    if(res.listTree){
                        setNewDetailPage(res.listTree)
                        treeNode.dataRef.children = treeNode.dataRef.children.concat(res.listTree)
                    }
                    this.treeData = [...this.treeData]
                    resolve()
                })
            })
        },
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
