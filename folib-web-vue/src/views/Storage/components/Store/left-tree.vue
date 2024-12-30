/**
功能：
作者：张佳宁
日期：
**/
<template>
    <div ref="container" class="left_tree_container">
        <div class="cover-box" v-if="isDragging"></div>
        <div ref="tree_container_sty" class="tree_container_sty" :style="{ height: topHeight + 'px' }">
            <a-tree 
                :replaceFields="{
                    key: 'artifactPath',
                    title: 'name',
                    children: 'children',
                }" 
                :tree-data="treeData" 
                class="leftTree"
                :load-data="(treeNode) => onLoadData(treeNode,false)" 
                @select="(key,e)=>treeSelect(key,e,false)" 
                @rightClick="onRightClick"
                @expand="(expandedKeys, treeNode) => onExpand(expandedKeys, treeNode, false)"
                :selectedKeys="selectedKeys"
                :expandedKeys="expandedKeys"
            >
                <a-icon slot="switcherIcon" type="down" />
                <a-icon slot="switcherIcon" type="folder-open" />
                <template slot="title" slot-scope="{ expanded,name,id,type,selected,fileType }">
                    <div class="title_box">
                        <span>
                            <a-icon class="tree_icon" style="margin-left: 5px;"
                                v-if="type === 'dir' || type === 'DIR'"
                                :type="expanded ? 'folder-open' : 'folder'" />
                            <a-icon class="tree_icon" style="margin-left: 10px;" v-else
                                :type="getIconType(name, type)"></a-icon>
                            <span class="tree_title">
                                {{ name }}
                            </span>
                        </span>
                    </div>
                </template>
            </a-tree>
        </div>
        <div class="line-box" :class="isDragging ? 'line-drag' : ''" @mousedown="startDragging"></div>
        <div ref="tree_container_sty" class="tree_container_sty recycle" :style="{ height: bottomHeight + 'px' }">
            <a-tree 
                :replaceFields="{
                    key: 'artifactPath',
                    title: 'name',
                    children: 'children',
                }" 
                :tree-data="trashData" 
                class="leftTree"
                :load-data="(treeNode) => onLoadData(treeNode,true)" 
                @select="(key,e)=>treeSelect(key,e,true)" 
                @rightClick="onRightClick" 
                @expand="(expandedKeys, treeNode) => onExpand(expandedKeys, treeNode, true)"
                :selectedKeys="selectRecycleKeys"
                :expandedKeys="expandedRecycleKeys"
            >
                <a-icon slot="switcherIcon" type="down" />
                <a-icon slot="switcherIcon" type="folder-open" />
                <template slot="title" slot-scope="{ expanded,name,id,type,selected,fileType,title }">
                    <div class="title_box">
                        <span>
                            <a-icon class="tree_icon" style="margin-left: 5px;"
                                v-if="type === 'dir' || type === 'DIR'"
                                :type="expanded ? 'folder-open' : 'folder'" />
                            <a-icon class="tree_icon" style="margin-left: 8px;" v-else :style="type == 'recycle'? 'color:#393b3e':''"
                                :type="getIconType(name, type)"></a-icon>
                            <span class="tree_title" :style="type == 'recycle'? 'color:#393b3e':''">
                                {{title || name }}
                            </span>
                        </span>
                    </div>
                </template>
            </a-tree>
        </div>
    </div>
</template>

<script>
export default {
    props: ['trashData', 'treeData'],
    data() {
        return {
            topHeight: 500, // 初始顶部 div 的高度 (容器高度 - 底部高度 - 分隔条高度)
            bottomHeight: 40, // 初始底部 div 的高度
            isDragging: false, // 是否正在拖拽
            containerHeight: 0, // 容器的总高度
            expandedKeys:[],
            selectedKeys:[],
            expandedRecycleKeys:[],
            selectRecycleKeys:[]
        }
    },
    computed: {
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
    },
    mounted() {
        this.$nextTick(() => {
            this.getPosition()
        })
    },
    methods: {
        onLoadData(treeNode,isTrashView) {
            return new Promise((resolve, reject) => {
                this.$emit('onLoadData', treeNode, isTrashView, resolve, reject);
            })
        },
        onExpand(expandedKeys, { node, expanded },key){
            if(node.dataRef.name === '.trash'){
                this.getPosition(expanded ? 300 : 40)
            }
            if(key){
                this.expandedRecycleKeys = expandedKeys
            }else{
                this.expandedKeys = expandedKeys
            }
        },
        treeSelect(key, e, type) {
            if(type){
                if(this.expandedRecycleKeys.length){
                    this.expandedRecycleKeys = []
                    this.getPosition()
                }else{
                    this.expandedRecycleKeys = ['.trash']
                    this.getPosition(300)
                }
                this.selectRecycleKeys = key
                this.selectedKeys = []
            }else{
                this.selectedKeys = key
                this.selectRecycleKeys = []
            }
            this.$emit('treeSelect',key, e)
        },
        onRightClick(params) {
            this.$emit('onRightClick',params)
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
            )

            // 如果顶部和底部高度都满足条件，则更新高度
            if (newTopHeight + newBottomHeight + 5 === this.containerHeight) {
                this.topHeight = newTopHeight;
                this.bottomHeight = newBottomHeight;
            }
        },
        stopDragging() {
            this.isDragging = false
            document.removeEventListener("mousemove", this.onMouseMove);
            document.removeEventListener("mouseup", this.stopDragging);
        },
        getPosition(bottomHeight = 40) {
            if(bottomHeight > this.bottomHeight || bottomHeight == 40){
                this.bottomHeight = bottomHeight
            }
            this.containerHeight = this.$refs.container.clientHeight - 5
            this.topHeight = this.containerHeight - this.bottomHeight - 5
        },
    }
}
</script>
<style lang="scss">
.left_tree_container {
    position: relative;
    height: 537px;
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

    .line-box {
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

.tree_container_sty {
    margin-left: 12px;
    margin-right: 12px;
    padding-top: 0px;
    overflow: hidden;

    &:hover {
        overflow: auto;
    }
}
</style>
<style lang="less" scoped>
@import url('./left-tree.less');
</style>
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
.leftTree.ant-tree li {
    padding: 2px 0px !important;
}

.leftTree .ant-tree-node-content-wrapper {
    width: 95% !important;
    height: 32px !important;
    line-height: 32px !important;
    border-radius: 8px !important;
}

.leftTree .ant-tree-switcher_close,
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

.leftTree .ant-tree-switcher-noop {
    display: none !important;
}

.leftTree .tree_icon {
    font-size: 16px;
    font-weight: 500;
}
</style>