<template>
    <div
        class="container-successes"
        v-if="isVisible"
        :class="containerClassName"
        :style="{ top: posY, left: posX }"
        @mousedown="startDrag"
        @dblclick="close"

    >
        <div class="wave-successes" :class="waveClassName"></div>
        <p class="cardTitle">{{ progress }}%</p>
    </div>
</template>

<script>
export default {
    props: {
        progress: {
            type: Number,
            default: 45,
        },
        closeGlobe:{
            type: Boolean,
            default: true
        },
        waveClassName: {
            type: String,
            default: 'wave-successes',
        },
        containerClassName: {
            type: String,
            default: 'container-successes',
        }
    },
    data() {
        return {
            isVisible: false,
            isDragging: false, // 是否正在拖动
            offsetX: 0, // 鼠标按下时的 X 偏移
            offsetY: 0, // 鼠标按下时的 Y 偏移
            posX: '98%', // 组件的 X 位置
            posY: '14%', // 组件的 Y 位置
        };
    },
    watch: {
        closeGlobe(newVal) {
            // 如果父组件传来的 closeGlobe 变为 true，则关闭当前组件
            if (newVal) {
                this.isVisible = newVal;
            }
        }
    },
    methods: {
        close() {
            this.isVisible = false; // 双击关闭组件
        },
        startDrag(event) {
            this.isDragging = true;
            // 记录鼠标按下时相对组件的位置
            this.offsetX = event.clientX - event.target.getBoundingClientRect().left+250;
            this.offsetY = event.clientY - event.target.getBoundingClientRect().top+60;
            document.addEventListener('mousemove', this.drag);
            document.addEventListener('mouseup', this.stopDrag);
        },
        drag(event) {
            if (this.isDragging) {
                // 更新组件的位置
                this.posX = `${event.clientX - this.offsetX}px`;
                this.posY = `${event.clientY - this.offsetY}px`;
            }
        },
        stopDrag() {
            this.isDragging = false;
            document.removeEventListener('mousemove', this.drag);
            document.removeEventListener('mouseup', this.stopDrag);
        },
    },
};
</script>

<style scoped>
.container-successes {
    position: absolute;
    width: 50px;
    height: 50px;
    padding: 5px;
    border: 2px solid rgb(2, 244, 70);
    top: 14%;
    left: 98%;
    transform: translate(-80%, -80%);
    border-radius: 50%;
    overflow: hidden;
}

.container-fail {
    position: absolute;
    width: 50px;
    height: 50px;
    padding: 5px;
    border: 2px solid rgb(244, 2, 67);
    top: 14%;
    left: 98%;
    transform: translate(-80%, -80%);
    border-radius: 50%;
    overflow: hidden;
}
.wave-successes {
    position: relative;
    width: 37px;
    height: 37px;
    background-color: rgb(2, 244, 15);
    border-radius: 50%;

    &::before,
    &::after{
        content: "";
        position: absolute;
        width: 100px;
        height: 100px;
        top: 0;
        left: 50%;
        background-color: rgba(255, 255, 255, .4);
        border-radius: 45%;
        transform: translate(-50%, -70%) rotate(0);
        animation: rotate 6s linear infinite;
        z-index: 1;
    }

    &::after {
        border-radius: 47%;
        background-color: rgba(255, 255, 255, .9);
        transform: translate(-50%, -70%) rotate(0);
        animation: rotate 10s linear -5s infinite;
        z-index: 2;
    }
}

.wave-fail {
    position: relative;
    width: 37px;
    height: 37px;
    background-color: rgb(244, 2, 2);
    border-radius: 50%;

    &::before,
    &::after{
        content: "";
        position: absolute;
        width: 100px;
        height: 100px;
        top: 0;
        left: 50%;
        background-color: rgba(255, 255, 255, .4);
        border-radius: 45%;
        transform: translate(-50%, -70%) rotate(0);
        animation: rotate 6s linear infinite;
        z-index: 1;
    }

    &::after {
        border-radius: 47%;
        background-color: rgba(255, 255, 255, .9);
        transform: translate(-50%, -70%) rotate(0);
        animation: rotate 10s linear -5s infinite;
        z-index: 2;
    }
}
.cardTitle {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    font-size: 14px;
    color: #000;
    z-index: 10;
}


@keyframes rotate {
    50% {
        transform: translate(-50%, -73%) rotate(180deg);
    }
    100% {
        transform: translate(-50%, -70%) rotate(360deg);
    }
}
</style>
