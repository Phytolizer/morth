; ModuleID = '.\assembly_src\dump.c'
source_filename = ".\\assembly_src\\dump.c"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc19.31.31106"

%struct._iobuf = type { i8* }

; Function Attrs: nounwind uwtable
define dso_local void @dump(i64 noundef %0) local_unnamed_addr #0 {
  %2 = alloca [32 x i8], align 16
  %3 = getelementptr inbounds [32 x i8], [32 x i8]* %2, i64 0, i64 0
  call void @llvm.lifetime.start.p0i8(i64 32, i8* nonnull %3) #4
  %4 = getelementptr inbounds [32 x i8], [32 x i8]* %2, i64 0, i64 31
  store i8 10, i8* %4, align 1, !tbaa !4
  br label %5

5:                                                ; preds = %5, %1
  %6 = phi i64 [ %0, %1 ], [ %14, %5 ]
  %7 = phi i64 [ 1, %1 ], [ %13, %5 ]
  %8 = urem i64 %6, 10
  %9 = trunc i64 %8 to i8
  %10 = or i8 %9, 48
  %11 = sub i64 31, %7
  %12 = getelementptr inbounds [32 x i8], [32 x i8]* %2, i64 0, i64 %11
  store i8 %10, i8* %12, align 1, !tbaa !4
  %13 = add i64 %7, 1
  %14 = udiv i64 %6, 10
  %15 = icmp ult i64 %6, 10
  br i1 %15, label %16, label %5, !llvm.loop !7

16:                                               ; preds = %5
  %17 = getelementptr inbounds [32 x i8], [32 x i8]* %2, i64 0, i64 %11
  %18 = tail call %struct._iobuf* @__acrt_iob_func(i32 noundef 1) #4
  %19 = call i64 @fwrite(i8* noundef nonnull %17, i64 noundef 1, i64 noundef %13, %struct._iobuf* noundef %18)
  call void @llvm.lifetime.end.p0i8(i64 32, i8* nonnull %3) #4
  ret void
}

; Function Attrs: argmemonly mustprogress nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: nofree nounwind
declare dso_local noundef i64 @fwrite(i8* nocapture noundef, i64 noundef, i64 noundef, %struct._iobuf* nocapture noundef) local_unnamed_addr #2

declare dso_local %struct._iobuf* @__acrt_iob_func(i32 noundef) local_unnamed_addr #3

; Function Attrs: argmemonly mustprogress nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #1

attributes #0 = { nounwind uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { argmemonly mustprogress nofree nosync nounwind willreturn }
attributes #2 = { nofree nounwind "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 2}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"uwtable", i32 1}
!3 = !{!"clang version 14.0.1"}
!4 = !{!5, !5, i64 0}
!5 = !{!"omnipotent char", !6, i64 0}
!6 = !{!"Simple C/C++ TBAA"}
!7 = distinct !{!7, !8}
!8 = !{!"llvm.loop.mustprogress"}
