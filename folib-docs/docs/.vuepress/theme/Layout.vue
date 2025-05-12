<template>
    <div id="app">
        <Navbar @getType="getType" />
        <div class="main-content"> 
            <Sidebar ref="Sidebar" :type="type" />
            <div class="content_box" ref="contentBox">
                <div class="breadcrumb_container">
                    <el-breadcrumb separator="/">
                        <el-breadcrumb-item style="font-weight: 600;" v-for="item,index in breadList" :key="index">
                            <span v-if="breadList.length-1 == index" style="color:#666;">
                                {{ item }}
                            </span>
                            <span v-else>
                                {{ item }}
                            </span>
                        </el-breadcrumb-item>
                    </el-breadcrumb>
                </div>
                <Content />
                <div class="pagination">
                    <router-link v-if="pagePrev" :style="pageNext ? '' : 'margin-right:50%'" :to="pagePrev.link" class="prev">
                        <span class="el-icon-back"></span>
                        <span class="page-text">
                            <span class="page-prev">
                                上一篇
                            </span>
                            <span class="page-title">
                                {{ pagePrev.title }}
                            </span>
                        </span> 
                    </router-link>
                
                    <router-link v-if="pageNext" :style="pagePrev ? '' : 'margin-left:50%'" :to="pageNext.link" class="next">
                        <span class="page-text">
                            <span class="page-next">
                                下一篇
                            </span>
                            <span class="page-title">
                                {{ pageNext.title }}
                            </span>
                        </span>
                        <span class="el-icon-right"></span>
                    </router-link>
                </div>
            </div>
            <TOC :tocItems="tocItems" ref="TOC" />
        </div>
    </div>
</template>

<script>
import Navbar from './components/Navbar.vue'
import Sidebar from './components/Sidebar.vue'
import TOC from './components/TOC.vue'

export default {
    components: { Navbar, Sidebar, TOC },
    data(){
        return {
            pageListItem:[],
            breadList:[],
            type:'productGuide'  // openApi
        }
    },
    computed: {
        tocItems() {
            // 去掉三级标题
            return this.$page.headers?.filter(ele => ele.level != 3).map(header => ({
                id: header.slug,
                text: header.title,
            }))
        },
        pagePrev(){
            const path = this.$page.path.split('.html')[0]
            const index = this.pageListItem.findIndex(page => page.link?.indexOf(path) != -1 )
            return index ? this.pageListItem[index - 1] : false
        },
        pageNext(){
            const path = this.$page.path.split('.html')[0]
            const index = this.pageListItem.findIndex(page => page.link?.indexOf(path) != -1 )
            const len = this.pageListItem.length - 1
            return index != len ? this.pageListItem[index + 1] : false
        }
    },
    watch:{
        $route(val){
            this.$refs.contentBox.scrollTo({
                top: 0,
                behavior: 'auto'
            })
            const path = val.path.split('.html')[0] // 去掉.html后缀
            this.getBreadList(path)
        },
        type(){
            this.getPageListItem(this.type)
            const path = this.$page.path.split('.html')[0]
            this.getBreadList(path)
            // 获取 contentBox 元素并绑定滚动事件
            this.$refs.contentBox.addEventListener('scroll', this.onScroll)
        }
    },
    mounted() {
        this.getPageListItem(this.type)
        const path = this.$page.path.split('.html')[0]
        this.getBreadList(path)
        // 获取 contentBox 元素并绑定滚动事件
        this.$refs.contentBox.addEventListener('scroll', this.onScroll)
    },
    beforeDestroy() {
        // 移除滚动事件监听器
        this.$refs.contentBox.removeEventListener('scroll', this.onScroll)
    },
    methods: {
        getType(type){
            this.type = type
        },
        getBreadList(path){
            this.breadList = []
            const pageList = this.$site.themeConfig.sidebar[this.type]
            pageList.forEach(ele => {
                ele.meta = ele.children?.map(el => el.link) || []
            })
            pageList.forEach(ele => {
                if(ele.meta.indexOf(path) != -1){
                    ele.children.forEach(el => {
                        if(el.children){
                            el.children.forEach(e => {
                                if(e.link == path){
                                    this.breadList.push(ele.title)
                                    this.breadList.push(el.title)
                                    this.breadList.push(e.title)
                                }
                            })
                        }else if(el.link && el.link == path){
                            this.breadList.push(ele.title)
                            this.breadList.push(el.title)
                        }
                    })
                }
            })
            console.log(this.breadList,'getBreadList')
        },
        getPageListItem(type = 'productGuide'){
            const pageList = this.$site.themeConfig.sidebar[type]
            const pageListItem = []
            function getPageList(list){
                if(!list){
                    return
                }
                list.forEach(ele => {
                    if(ele.children){
                        getPageList(ele.children)
                    }else{
                        pageListItem.push(ele)
                    }
                })
            }
            getPageList(pageList)
            this.pageListItem = pageListItem
        },
        onScroll(){
            const contentBox = this.$refs.contentBox;
            const scrollPosition = contentBox.scrollTop 
            const isAtBottom = contentBox.scrollTop + contentBox.clientHeight >= contentBox.scrollHeight;
            this.$refs.TOC.updateActiveHeading(scrollPosition,isAtBottom)
        }
    },
}
</script>

<style lang="scss">
#app {
    display: flex;
    flex-direction: column;
    /* 确保子元素按列排列 */
    height: 100vh;
}

.main-content {
    display: flex;
}

main {
    flex-grow: 0;
}

.content_box {
    position: relative;
    padding-right: 240px;
    height: calc(100vh - 72px);
    overflow-x: hidden;
    .content__default{
        position: relative;
        width: calc(100vw - 288px - 250px);
        padding: 60px;
        padding-top: 50px;
        padding-bottom: 120px;
        box-sizing: border-box;
        border-left: 1px solid #e4e6e8;
        border-right: 1px solid #e4e6e8;

        p img{
            width: 100% !important;
            border-radius: 8px;
        }
        img.half{
            width: 50% !important;
        }
    }

    .pagination{
        position: absolute;
        width: calc(100vw - 288px - 250px);
        height: 64px;
        margin-top: -100px;
        display: flex;
        justify-content: space-around;

        .prev,.next{
            cursor: pointer;
            display: flex;
            // display: inline-block;
            justify-content: space-between;
            padding-left: 30px;
            padding-right: 30px;
            align-items: center;
            width: calc(50% - 180px);
            height: 64px;
            font-size: 14px;
            color: #666;
            background: #fff;
            box-shadow: 0 4px 12px 0 rgba(32,45,64,.08),0 1px 3px 0 rgba(32,45,64,.05);
            transition: all 0.2s;
            border-radius: 4px;

            &:hover{
                box-shadow: 0 8px 24px 0 rgba(32,45,64,.14),0 1px 3px 0 rgba(32,45,64,.05);

                .page-title{
                    color: #0A6AFF;
                }
            }

            .page-text{
                display: flex;
                flex-direction: column;
                justify-content: space-between;
                line-height: 25px;
            }
        }

        .prev{
            .el-icon-back{
                font-size: 18px;
                font-weight: 600;
            }

            .page-prev{
                text-align: right;

            }
        }   
        .next{
            .el-icon-right{
                font-size: 18px;
                font-weight: 600;
            }
            .page-next{
                text-align: left;
            }
        }
    }

    .breadcrumb_container{
        position: fixed;
        left: 300px;
        padding-top: 30px;
        padding-bottom: 20px;
        padding-left: 50px;
        top: 72px;
        width: calc(100% - 615px);
        background: #fff;
        z-index: 19;

        .el-breadcrumb__item{

            .el-breadcrumb__inner{
                font-size: 14px;
                color: #0A6AFF !important;
            }
        }
    }
}
</style>