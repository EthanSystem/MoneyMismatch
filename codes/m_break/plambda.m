function lambda=plambda(b,maxStructuralChanges,numEffectiveSample)

% procedure that construct a diagonal matrix of dimension maxStructuralChanges+1 with ith
% entry (T_i-T_i-1)/T.

lambda=zeros(maxStructuralChanges+1,maxStructuralChanges+1);
lambda(1,1)=b(1,1)/numEffectiveSample;
k=2;
while k <= maxStructuralChanges
    lambda(k,k)=(b(k,1)-b(k-1,1))/numEffectiveSample;
    k=k+1;
end
lambda(maxStructuralChanges+1,maxStructuralChanges+1)=(numEffectiveSample-b(maxStructuralChanges,1))/numEffectiveSample;
