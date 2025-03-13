/**
功能：
作者：张佳宁
日期：
**/
<template>
    <div class="left_tree-container" ref="container">
        <div class="cover-box" v-if="isDragging"></div>
        <a-spin :spinning="loadingMore">
            <div ref="tree_container_store" class="tree_container_store" :style="{ height: topHeight + 'px' }"
                @scroll="handleScroll">
                <vue-easy-tree
                    :key="key"
                    class="repositoryTree"
                    ref="storeTree"
                    node-key="key"
                    :props="props"
                    icon-class="el-icon-arrow-right"
                    lazy
                    :height="`${topHeight}px`"
                    :load="(treeNode,resolve) => onLoadData(treeNode, resolve, false)"
                    :data="treeData"
                    :show-line="true"
                    @node-click="(data)=>treeSelect(data,false)"
                    @node-expand="(data,node) => onExpand(data, node, false)"
                    @node-collapse="(data,node) => onCollapse(data,node, false)"
                    @node-contextmenu="(event, data) => rightClick(event, data,'tree')"
                    :expandedKeys="expandedKeys"
                    show-icon
                    :selectedKeys="selectedKeys"
                >
                    <template slot-scope="{data,node}">
                        <div class="title_box">
                        <span>
                            <a-icon v-if="node.loading && !node.expanded" type="loading" :style="{color: '#1890ff'}"/>
                            <img v-if="data.fileType === 'document'" :src="getSrc(data.selected, data.type)" alt="" width="24">
                            <span v-if="data.fileType === 'document'" class="tree_title">
                                {{ data.id }}
                            </span>
                            <span v-else>
                                <a-icon class="tree_icon" style="margin-left: 5px;"
                                        v-if="data.type === 'dir' || data.type === 'DIR'"
                                        :type="node.expanded ? 'folder-open' : 'folder'" />
                                <a-icon class="tree_icon" style="margin-left: 10px;" v-else
                                        :type="getIconType(data.name, data.type)"></a-icon>
                                <span class="tree_title">
                                    {{ data.name }}
                                </span>
                            </span>
                        </span>
                        </div>
                    </template>
                </vue-easy-tree>
            </div>
        </a-spin>
        <div class="line" :class="isDragging ? 'line-drag' : ''" @mousedown="startDragging"></div>
        <a-spin :spinning="loadingMore">
            <div ref="tree_container" class="tree_container recycle" :style="{ height: bottomHeight + 'px' }"
                @scroll="handleScroll">
                <vue-easy-tree
                    :key="recycleKey" 
                    class="repositoryTree" 
                    ref="recycleTree"
                    node-key="key"
                    :props="props"
                    icon-class="el-icon-arrow-right"
                    lazy
                    :height="`${bottomHeight}px`"
                    :load="(treeNode,resolve) => onLoadData(treeNode, resolve, true)"
                    :data="recycleTreeData"
                    :show-line="true"
                    @node-click="(data)=>treeSelect(data,true)"
                    @node-expand="(data,node) => onExpand(data,node, true)"
                    @node-collapse="(data,node) => onCollapse(data,node, true)"
                    @node-contextmenu="(event, data) => rightClick(event, data,'recycleTree')"
                    :expandedKeys="expandedRecycleKeys"
                    show-icon
                    :selectedKeys="selectRecycleKeys"
                >
                    <template slot-scope="{data,node}">
                        <div class="title_box">
                        <span>
                            <a-icon v-if="node.loading && !node.expanded" type="loading" :style="{color: '#1890ff'}"/>
                            <img v-if="data.fileType === 'document'" :src="getSrc(data.selected, data.type)" alt="" width="24">
                            <span v-if="data.fileType === 'document'" class="tree_title">
                                {{ data.id }}
                            </span>
                            <span v-else>
                                <a-icon class="tree_icon" style="margin-left: 5px;"
                                        v-if="data.type === 'dir' || data.type === 'DIR'"
                                        :type="node.expanded ? 'folder-open' : 'folder'" />
                                <a-icon class="tree_icon" style="margin-left: 10px;" v-else :style="data.type === 'recycle'? 'color:#393b3e':''"
                                        :type="getIconType(data.name, data.type)"></a-icon>
                                <span class="tree_title" :style="data.type === 'recycle'? 'color:#393b3e':''">
                                    {{ data.name }}
                                </span>
                            </span>
                        </span>
                        </div>
                    </template>
                </vue-easy-tree>
            </div>
        </a-spin>
        <rightMenu 
            ref="rightMenu" 
            v-show="showContextMenu" 
            :style="contextMenuStyle" 
            :folibRepository="folibRepository"
            :repositoryType="repositoryType" 
            :currentFileDetial="currentFileDetial" 
            :uploadEnabled="uploadEnabled"
            :copyEnabled='copyEnabled' 
            :dispatchEnabled="dispatchEnabled" 
            :moveEnabled="moveEnabled"
            :showContextMenu="showContextMenu"
            :currentTreeNode="isTrashView ? currentTreeNodeRecycle : currentTreeNode"
            :isTrashView="isTrashView" 
            @reload="reload" 
            @localDelNode="localDelNode"
            @handleMenuClick="handleMenuClick" 
        />
    </div>
</template>
<script>
import local from './images/local.svg'
import localCheck from './images/local-check.svg'
import remote from './images/remote.svg'
import remoteCheck from './images/remote-check.svg'
import virtual from './images/virtual.svg'
import virtualCheck from './images/virtual-check.svg'
import { getDockerArtifact, browse, getArtifact, getStorageAndRepositoryPermission, getArtifactPermission } from '@/api/folib'
import { getLayoutType } from '@/utils/layoutUtil'
import rightMenu from './right-menu.vue'
import { hasRole, isAdmin } from '@/utils/permission'
import VueEasyTree from "@wchbrad/vue-easy-tree"
import "@wchbrad/vue-easy-tree/src/assets/index.scss"

export default {
    components: {
        rightMenu,
        VueEasyTree
    },
    props: ['repositories', 'storageId'],
    data() {
        return {
            treeData: [],
            folibRepository: {},
            props: {
                label: 'name',
                children: 'children',
                isLeaf: 'isLeaf'
            },
            key: 0,
            repositoryType: '',
            artifactPath: '',
            loadingMore: false,
            selectedKeys: [],
            selectRecycleKeys: [],
            currentFileDetial: {},
            permissions: [],
            uploadEnabled: false,
            copyEnabled: false,
            dispatchEnabled: false,
            moveEnabled: false,
            showContextMenu: false,
            currentTreeNode: {},
            currentTreeNodeRecycle: {},
            rightClickTop: '0px',
            rightClickLeft: '0px',
            enablUploadedLayout: ['Raw', 'php', 'Maven 2', 'npm', 'rpm', 'go', 'GitLfs', 'pub', 'debian'],
            expandedKeys: [],
            expandedRecycleKeys: [],
            topHeight: 500, // 初始顶部 div 的高度 (容器高度 - 底部高度 - 分隔条高度)
            bottomHeight: 40, // 初始底部 div 的高度
            isDragging: false, // 是否正在拖拽
            containerHeight: 0, // 容器的总高度
            recycleTreeData:[{
                name:'制品回收站',
                icon:'',
                type:'recycle',
                children:[],
                key:0
            }],
            isTrashView:false,
            recycleRepositryList:[],
            recycleKey:0
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
        getIconType() {
            return (name, type) => {
                const _name = name.toLowerCase()
                const _type = type.toLowerCase()
                let icon = ''
                if (_type === 'file') {
                    icon = 'file'
                }
                if (_name.indexOf('.png') !== -1) {
                    icon = 'file-image'
                }
                if (_name.indexOf('.zip') !== -1) {
                    icon = 'file-zip'
                }
                if (_name.indexOf('.md') !== -1) {
                    icon = 'file-markdown'
                }
                if (_name.indexOf('.pdf') !== -1) {
                    icon = 'file-pdf'
                }
                if(_type === 'recycle'){
                    icon = 'delete'
                }
                return icon
            }
        },
        contextMenuStyle() {
            return {
                position: 'fixed',
                top: this.rightClickTop,
                left: this.rightClickLeft,
            }
        },
        treeClickKey() {
            return this.$store.state.treeClickKey
        },
        restoreActionMark() {
            return this.$store.state.restoreActionMark
        }
    },
    watch: {
        repositories: {
            handler(val) {
                if (val) {
                    if (val.length && !this.treeData.length) {
                        this.$nextTick(() => {
                            const nodeList = document.querySelectorAll('.vue-recycle-scroller')
                            nodeList.forEach(item => {
                                item.addEventListener('scroll', this.handleScroll);
                            })
                        })
                    }
                    this.isTrashView = false
                    this.loadingMore = false
                    this.loadingMoreShow(false)
                    let key = true
                    if(this.treeData.length){
                        this.treeData.forEach(ele => {
                            val.forEach(el => {
                                if(ele.id === el.id){
                                    key = false
                                }
                            })
                        })
                    }
                    if(key){
                        this.treeData = this.treeData.concat(JSON.parse(JSON.stringify(val)))
                        this.recycleRepositryList = this.recycleRepositryList.concat(JSON.parse(JSON.stringify(val)))
                        // function uniqueById(arr) {
                        //     const unique = [];
                        //     const seen = new Set();
                        //     arr.forEach(item => {
                        //         if (!seen.has(item.id)) {
                        //         unique.push(item);
                        //         seen.add(item.id);
                        //         }
                        //     });
                        //     return unique;
                        // }
                        // this.treeData = uniqueById(this.treeData)
                        const objs = ['treeData','recycleRepositryList']
                        objs.forEach(key => {
                            this[key].forEach(ele => {
                                ele.fileType = 'document'
                                ele.key = ele.id
                                ele.name = ele.id
                                ele.artifactPath = ''
                                ele.newDetailPage = true
                                ele.treeType = 'root'
                            })
                        })
                        if (!this.treeData.length) {
                            return
                        }
                        this.getRecycleTreeData()
                        this.selectedKeys = [this.treeData[0].id]
                        this.treeSelect(this.treeData[0])
                    }
                }
            },
            immediate: true
        },
        storageId(val) {
            if (val) {
                // 切换不同的存储空间时，清空原有的树结构
                this.key++
                this.recycleKey ++
                this.empty()
                this.loadingMoreShow(true)
            }
        },
        restoreActionMark() {
            const copyNode = JSON.parse(JSON.stringify(this.currentTreeNodeRecycle))
            this.localDelNode(copyNode, true)
            this.localRestoreNode(copyNode)
        },
    },
    mounted() {
        document.addEventListener('click', this.closeContextMenu)
        this.$nextTick(() => {
            this.getPosition()
        })
        this.recycleTreeData = [{
            name:'制品回收站',
            icon:'',
            type:'recycle',
            children:[],
            key:this.storageId || '-'
        }]
    },
    beforeDestroy() {
        document.removeEventListener('click', this.closeContextMenu)
        const nodeList = document.querySelectorAll('.vue-recycle-scroller')
        nodeList.forEach(item => {
            item.removeEventListener('scroll', this.handleScroll);
        })
    },
    methods: {
        reload(key = false) {
            this.recycleKey ++
            if(key){
                this.getRecycleTreeData()
            }
            if(!key){
                this.recycleRepositryList = []
                this.treeData = []
                this.$emit('getDetailInfo', true, 'repositoryTree')
            }
        },
        // 远程节点删除成功后，调用本地删除节点方法（实现删除后树表展开结构不变）
        localDelNode(tarNode, isTrashView = false) {
            const sourceArr = isTrashView ? this.recycleTreeData : this.treeData
            const currentTreeRef =  isTrashView ? this.$refs.recycleTree : this.$refs.storeTree
            const { id } = this.folibRepository
            const { artifactPath } = tarNode
            const parentKeyArr = artifactPath.split('/')
            parentKeyArr.pop()
            let parentKey = `${id}${parentKeyArr.join('/')}`
            let updatedChildren = []
            const recursionGetChildren = (source, artifactPath) => {
                source.forEach(item => {
                    if (item.artifactPath === artifactPath) {
                        updatedChildren = source.filter(ele => ele.artifactPath !== artifactPath)
                    } else if (item.children?.length) {
                        recursionGetChildren(item.children, artifactPath)
                    }
                })
            }
            recursionGetChildren(sourceArr, artifactPath)
            currentTreeRef.updateKeyChildren(parentKey, updatedChildren)
        },
        // 远程节点从回收站还原成功后，调用本地新增节点方法（实现删除后树表展开结构不变）
        localRestoreNode(tarNode) {
            const { id } = this.folibRepository
            const { artifactPath } = tarNode
            const parentKeyArr = artifactPath.split('/')
            parentKeyArr.shift()
            parentKeyArr.pop()
            let parentKey = `${id}${parentKeyArr.join('/')}`
            const updateNode = {
                artifactPath: parentKeyArr.join('/'),
                key: parentKey
            }
            console.log(updateNode);
            this.handleRefresh(updateNode)
        },
        // 右键菜单选择操作
        handleMenuClick(active) {
            if (active.key === '1') {
                this.handleRefresh()
                return
            }
            this.$emit('handleMenuClick', active, this.isTrashView ? this.currentTreeNodeRecycle : this.currentTreeNode, this.folibRepository)
        },
        // 递归根据key判断是否为当前选中数据项
        recursionGetItems(source, key, children, loading) {
            source.forEach(item => {
                if (item.key === key) {
                    if (children) this.$refs.tree.updateKeyChildren(key, children)
                    item.loading = loading
                } else if (item.children?.length) {
                    this.recursionGetItems(item.children, key, children, loading)
                }
            })
        },
        handleRefresh(refreshNode) {
            // const nowDataKey = this.isTrashView ? 'recycleTreeData' : 'treeData'
            const { storageId, id, layout } = this.folibRepository
            const currentNode = refreshNode || (this.isTrashView ? this.currentTreeNodeRecycle : this.currentTreeNode)
            const currentTreeRef =  this.isTrashView && !refreshNode ? this.$refs.recycleTree : this.$refs.storeTree
            // this.recursionGetItems(this[nowDataKey], currentNode.key, null, true)
            if (layout === 'Docker') {
                getDockerArtifact(
                    storageId,
                    id,
                    currentNode.artifactPath
                ).then(res => {
                    const children = []
                    if (res.directories.length > 0) {
                        let f = res.directories
                        if (res.directories.some(ele => ele.name === '.trash')) {
                            if (this.isTrashView) {
                                f = res.directories.filter(ele => ele.name === '.trash')
                            } else {
                                f = res.directories.filter(ele => ele.name !== '.trash')
                            }
                        }
                        const d = f
                        d.forEach((item, index, d) => {
                            item.type = 'dir'
                            item.isLeaf = false
                            item.key = id + item.artifactPath
                            children.push(item)
                        })
                    }
                    if (res.files.length > 0 && (((this.isTrashView && currentNode.fileType !== 'document') || !this.isTrashView) || refreshNode)) {
                        const a = res.files
                        a.forEach((item, index, a) => {
                            item.isLeaf = !this.getFileIsOpen(item.name)
                            item.type = 'file'
                            item.key = id + item.artifactPath
                            children.push(item)
                        })
                    }

                    currentTreeRef.updateKeyChildren(currentNode.key, children)
                    // this.recursionGetItems(this[nowDataKey], currentNode.key, children, false)
                    // this.setKeyValue()
                })
                return
            }
            browse(
                storageId,
                id,
                currentNode.artifactPath
            ).then(res => {
                let children = []
                if (res.directories.length > 0) {
                    let f = res.directories
                    if (res.directories.some(ele => ele.name === '.trash')) {
                        if (this.isTrashView) {
                            f = res.directories.filter(ele => ele.name === '.trash')
                        } else {
                            f = res.directories.filter(ele => ele.name !== '.trash')
                        }
                    }
                    const d = f
                    d.forEach((item, index, d) => {
                        item.type = 'dir'
                        item.key = id + item.artifactPath
                    })
                    children = d
                }
                if (res.files.length > 0 && (((this.isTrashView && currentNode.fileType !== 'document') || !this.isTrashView) || refreshNode)) {
                    const a = res.files
                    a.forEach((item, index, a) => {
                        item.isLeaf = !this.getFileIsOpen(item.name)
                        item.type = 'file'
                        item.key = id + item.artifactPath
                    })
                    children = children.concat(a)
                }
                currentTreeRef.updateKeyChildren(currentNode.key, children)
                // this.recursionGetItems(this[nowDataKey], currentNode.key, children, false)
                // this.setKeyValue()
            })
        },
        setKeyValue() {
            this.key++
        },
        // 鼠标右键
        rightClick(event, data, type) {
            this.showContextMenu = false
            this.isTrashView = type === 'recycleTree'
            const { treeType } = data
            if (treeType === 'lastRoot') return
            const callback = () => {
                this.showContextMenu = true
                this.$nextTick(() => {
                    const viewportHeight = window.innerHeight;
                    const spaceBelow = viewportHeight - event.clientY;
                    const menuHeight = this.$refs.rightMenu?.$el?.offsetHeight;
                    let top = event.clientY;
                    if (spaceBelow < menuHeight) {
                        top = event.clientY - menuHeight;
                    }
                    this.rightClickTop = `${top}px`;
                    this.rightClickLeft = `${event.clientX}px`;
                })
            }
            if(this.isTrashView){
                this.currentTreeNodeRecycle = data
            }else{
                this.currentTreeNode = data
            }
            let target = null
            // 获取当前子节点的最顶层父节点（仓库节点）
            this.treeData.forEach(ele => {
                if (ele.id === data.repositoryId || ele.id === data.id) {
                    target = ele
                }
            })
            this.folibRepository = target
            this.repositoryType = getLayoutType(this.folibRepository)
            if (this.folibRepository.type === 'proxy') {
                this.uploadEnabled = false
                this.copyEnabled = false
                this.dispatchEnabled = false
                this.moveEnabled = false
            } else {
                this.queryPermission()
            }
            this.$refs.rightMenu.handlerDataPermission(data,type, callback)
            if (target.layout && data.type === 'file') {
                const params = {
                    storageId: target.storageId,
                    id: target.id,
                    artifactPath: data.artifactPath
                }
                this.getPackagePreview(params)
            }
        },
        closeContextMenu() {
            this.showContextMenu = false;
        },
        empty() {
            this.treeData = []
            this.recycleRepositryList = []
            this.recycleTreeData = [{
                name:'制品回收站',
                icon:'',
                type:'recycle',
                children:[],
                key:this.storageId
            }]
            this.expandedRecycleKeys = []
            this.getPosition()
        },
        // 设置loading状态
        loadingMoreShow(key) {
            this.loadingMore = key
            // 设置只有在请求结束后才可以切换模式，方式快速切换导致数据渲染错误
            this.$store.commit('setSwitchDisabled', key)
        },
        // 滚动条滚动
        handleScroll(event) {
            const { scrollTop, clientHeight, scrollHeight } = event.target;
            if (scrollTop + clientHeight + 10 >= scrollHeight) {
                this.$nextTick(() => {
                    const total = this.treeData.length
                    this.$emit('loadMore', total)
                })
            }
        },
        // 判断那些文件类型是可以打开的
        getFileIsOpen(name) {
            const _name = name.toLowerCase()
            const tarArr = ['.tar', '.jar', '.zip', '.7z', '.tar.gz', 'tgz']
            let key = false
            tarArr.forEach(ele => {
                if (_name.indexOf(ele) !== -1) {
                    key = true
                }
            })
            return key
        },
        // 树节点点击
        treeSelect(data, isRecycle) {
            this.closeContextMenu()
            const { newDetailPage,name } = data
            if(isRecycle){
                if(name == '制品回收站'){
                    if(this.expandedRecycleKeys.length){
                        this.expandedRecycleKeys = []
                        this.getPosition()
                    }else{
                        this.expandedRecycleKeys = [this.storageId]
                        this.getPosition(320)
                    }
                    this.treeSelect(this.recycleRepositryList[0],true)
                    return
                }else{
                    this.selectRecycleKeys = [data.key]
                }
                this.selectedKeys = []
            }else{
                this.selectedKeys = [data.key]
                this.selectRecycleKeys = []
            }
            this.$store.commit('setNewDetailPage', !!newDetailPage)
            if (data.fileType === 'document') {
                this.folibRepository = data
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
                this.$emit('repositorySelect', data)
            } else {
                let target = null
                // 获取当前子节点的最顶层父节点（仓库节点）
                this.treeData.forEach(ele => {
                    if (ele.id === data.repositoryId) {
                        target = ele
                    }
                })
                this.folibRepository = target
                // this.$emit('repositorySelect', data)
                this.$nextTick(() => {
                    setTimeout(() => {
                        this.$emit('treeSelect', data, isRecycle, target)
                    }, 0);
                })
            }
            // 如果为新页面（jar包下面的文件夹），更新右侧展示页面数据
            if (!!newDetailPage) {
                const { id, storageId } = this.folibRepository
                let params = data
                params.repositoryId = id
                params.storageId = storageId
                this.$store.commit('setCurrentTreeNode', params)
            }
        },
        // 获取当前已展开节点的key
        onExpand(data,node,key) {
            this.closeContextMenu()
            // if(key){
            //     this.expandedRecycleKeys = [data.key]
            // }else{
            //     this.expandedKeys = [data.key]
            // }
            // if(key && data.name === '制品回收站'){ // 回收站打开
            //     this.getPosition(320)
            // }
        },
        onCollapse(data,node,key) {
            this.closeContextMenu()
            if(key && data.name === '制品回收站'){ // 回收站关闭
                this.getPosition(40)
            }
        },
        getRecycleTreeData(){
            let list = []
            this.recycleRepositryList.forEach(ele => {
                list.push(JSON.parse(JSON.stringify(ele)))
            })
            this.recycleTreeData[0].children = [...list]
            this.recycleTreeData = [...this.recycleTreeData]
        },
        // 懒加载获取节点
        onLoadData(treeNode,resolve, isTrashView) {
            this.closeContextMenu()
            if (!treeNode.data.fileType && !treeNode.data.type) return
            if(treeNode.data.type === 'recycle'){
                this.getPosition(320)
            }
            if (treeNode.data.fileType === 'document') {
                this.folibRepository = treeNode.data
                this.repositoryType = getLayoutType(this.folibRepository)
                this.queryPermission()
            } else if (treeNode.data.type !== 'recycle'){
                let target = null
                // 获取当前子节点的最顶层父节点（仓库节点）
                this.treeData.forEach(ele => {
                    if (ele.id === treeNode.data.repositoryId) {
                        target = ele
                    }
                })
                this.folibRepository = target
            }
            const { storageId, id, layout } = this.folibRepository
            const { artifactPath, name } = treeNode.data
            const params = {
                treeNode,
                storageId,
                id,
                layout,
                artifactPath,
                name
            }
            if (this.getFileIsOpen(name)) {
                this.getPackagePreview(params, resolve)
                return
            }

            if (layout === 'Docker') {
                if (treeNode.data.children) {
                    resolve(treeNode.data.children)
                    return
                }
                getDockerArtifact(
                    storageId,
                    id,
                    artifactPath
                ).then(res => {
                    treeNode.data.children = []
                    if (res.directories.length > 0) {
                        let f = res.directories
                        if (res.directories.some(ele => ele.name === '.trash')) {
                            if (isTrashView) {
                                f = res.directories.filter(ele => ele.name === '.trash')
                            } else {
                                f = res.directories.filter(ele => ele.name !== '.trash')
                            }
                        }
                        const d = f
                        d.forEach((item, index, d) => {
                            item.type = 'dir'
                            item.key = id + item.artifactPath
                            treeNode.data.children.push(item)
                        })
                    }
                    if (res.files.length > 0 && ((isTrashView && treeNode.data.fileType !== 'document') || !isTrashView)) {
                        const a = res.files
                        a.forEach((item, index, a) => {
                            item.isLeaf = !this.getFileIsOpen(item.name)
                            item.type = 'file'
                            item.key = id + item.artifactPath
                            treeNode.data.children.push(item)
                        })
                    }
                    resolve(treeNode.data.children)
                    // if (treeNode.data.artifactPath) return
                    // this[nowDataKey] = [...this[nowDataKey]]
                })
                return
            }

            if (treeNode.data.children) {
                resolve(treeNode.data.children)
                return
            }
            browse(
                storageId,
                id,
                artifactPath
            ).then(res => {
                if (!treeNode.data.children) {
                    treeNode.data.children = []
                }
                if (res.directories.length > 0) {
                    let f = res.directories
                    if (res.directories.some(ele => ele.name === '.trash')) {
                        if (isTrashView) {
                            f = res.directories.filter(ele => ele.name === '.trash')
                        } else {
                            f = res.directories.filter(ele => ele.name !== '.trash')
                        }
                    }
                    const d = f
                    d.forEach((item, index, d) => {
                        item.type = 'dir'
                        item.key = id + item.artifactPath
                    })
                    treeNode.data.children = d
                }
                if (res.files.length > 0 && ((isTrashView && treeNode.data.fileType !== 'document') || !isTrashView)) {
                    const a = res.files
                    a.forEach((item, index, a) => {
                        item.isLeaf = isTrashView || !this.getFileIsOpen(item.name)
                        item.type = 'file'
                        item.key = id + item.artifactPath
                    })
                    treeNode.data.children = treeNode.data.children.concat(a)
                }
                resolve(treeNode.data.children)
                // if (treeNode.data.artifactPath) return
                // this[nowDataKey] = [...this[nowDataKey]]
            })
        },
        // 获取可以继续打开的文件的目录（对应包预览）
        getPackagePreview({ treeNode, storageId, id, artifactPath }, resolve) {
            if (treeNode?.data.children) {
                resolve(treeNode.data.children)
                return
            }
            getArtifact(
                this.repositoryType,
                storageId,
                id,
                artifactPath
            ).then(res => {
                this.currentFileDetial = res
                if (!resolve) return
                function setNewDetailPage(arr) {
                    arr.forEach(ele => {
                        ele.newDetailPage = true
                        ele.treeType = 'lastRoot'
                        ele.storageId = storageId
                        ele.repositoryId = id
                        ele.key = ele.name
                        ele.artifactPath = `${id}/${artifactPath}/${ele.name}`
                        if (ele?.children?.length) {
                            setNewDetailPage(ele.children)
                        }
                    })
                }
                treeNode.data.children = []
                if (res.listTree) {
                    setNewDetailPage(res.listTree)
                    treeNode.data.children = treeNode.data.children.concat(res.listTree)
                }
                // this[nowDataKey] = [...this[nowDataKey]]
                resolve(treeNode.data.children)
            })
        },
        queryPermission() {
            this.storageAdmin = ""
            this.permissions = []
            getStorageAndRepositoryPermission(
                this.folibRepository.storageId,
                this.folibRepository.id
            ).then(res => {
                this.storageAdmin = res.storageAdmin
                this.permissions = res.permissions || []
                this.uploadEnabled =
                    this.folibRepository.status.indexOf('Out of Service') === -1 &&
                    this.enablUploadedLayout.includes(this.folibRepository.layout) &&
                    (this.folibRepository.type === 'hosted' || (this.folibRepository.type === 'group' && this.folibRepository.groupDefaultRepository)) &&
                    (hasRole('ARTIFACTS_MANAGER') ||
                        this.permissions.includes('ARTIFACTS_DEPLOY'))
                this.copyEnabled = this.folibRepository.type === 'hosted' && (hasRole('ARTIFACTS_MANAGER') || this.permissions.includes('ARTIFACTS_COPY'))
                this.dispatchEnabled = this.folibRepository.type === 'hosted' && isAdmin()
                this.moveEnabled = this.folibRepository.type === 'hosted' && (hasRole('ARTIFACTS_MANAGER') || this.permissions.includes('ARTIFACTS_MOVE'))
            })
        },
        startDragging(event) {
            event.preventDefault()
            this.isDragging = true
            document.addEventListener("mousemove", this.onMouseMove)
            document.addEventListener("mouseup", this.stopDragging)
        },
        onMouseMove(event) {
            if (!this.isDragging) return

            const containerRect = this.$el.getBoundingClientRect()
            const offsetY = event.clientY - containerRect.top

            // 限制顶部和底部高度不能小于 40px
            const newTopHeight = Math.max(40, offsetY)
            const newBottomHeight = Math.max(
                40,
                this.containerHeight - newTopHeight - 5 // 计算底部 div 高度
            );

            // 如果顶部和底部高度都满足条件，则更新高度
            if (newTopHeight + newBottomHeight + 5 === this.containerHeight) {
                this.topHeight = newTopHeight
                this.bottomHeight = newBottomHeight
            }
        },
        stopDragging() {
            this.isDragging = false
            document.removeEventListener("mousemove", this.onMouseMove)
            document.removeEventListener("mouseup", this.stopDragging)
        },
        getPosition(bottomHeight = 40){
            if(bottomHeight > this.bottomHeight || bottomHeight == 40){
                this.bottomHeight = bottomHeight
            }
            this.containerHeight = this.$refs.container.clientHeight - 5
            this.topHeight = this.containerHeight - this.bottomHeight - 5
        }
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
<style lang="scss">
.repositoryTree.ant-tree li {
    padding: 2px 0px !important;
}

.repositoryTree .ant-tree-node-content-wrapper {
    width: 90% !important;
    height: 32px !important;
    line-height: 32px !important;
    border-radius: 8px !important;
}

.repositoryTree .ant-tree-switcher_close,
.ant-tree-switcher_open {
    height: 32px !important;
    line-height: 32px !important;
}

.tree_container {
    margin-left: 12px;
    margin-right: 12px;
    padding-top: 0px;
    overflow: hidden;

    &:hover {
        overflow: auto;
    }
}

.repositoryTree .ant-tree-switcher-noop {
    display: none !important;
}

.repositoryTree .tree_icon {
    font-size: 16px;
    font-weight: 500;
}
</style>

<style lang="less" scoped>
@import url('./repository-tree.less');
</style>
<style lang="scss">
.left_tree-container {
    position: relative;
    height: calc(100vh - 282px);
    display: flex;
    flex-direction: column;

    .cover-box {
        height: 100%;
        width: 100%;
        position: absolute;
        cursor: s-resize;
        left: 0;
        top: 0;
        z-index: 10;
    }

    .line {
        position: relative;
        z-index: 11;
        height: 5px;
        transition: all 0.5s;
        background: transparent;
        transition-delay: 0.2s;
        border-bottom: 1px solid #ccc;
        cursor: s-resize;

        &:hover {
            border-bottom: 1px solid transparent;
            background: #1890ff;
        }

        &.line-drag {
            border-bottom: 1px solid transparent;
            background: #1890ff;
        }
    }
}

.recycle {
    margin-top: 5px;
}

.vue-recycle-scroller.direction-vertical:not(.page-mode) {
    overflow-x: auto;
}
.vue-recycle-scroller__item-wrapper {
    overflow: visible;
}
</style>
